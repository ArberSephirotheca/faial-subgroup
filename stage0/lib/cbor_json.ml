(* SPDX-License-Identifier: MIT
   Adapted from ocaml-cbor 0.5 (https://github.com/ygrek/ocaml-cbor),
   src/CBOR.ml. The upstream notice is preserved verbatim:

   ----------------------------------------------------------------------
   The MIT License (MIT)

   Copyright (c) 2014 ygrek

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject
   to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------

   Changes from upstream:
   - The decoder constructs [Yojson.Basic.t] directly, skipping the
     intermediate [CBOR.Simple.t] tree.
   - The encoder and diagnostic printer are dropped (unused in faial).
   - Errors are surfaced as [Result.Error] at the API boundary; the
     parser body uses exceptions internally for terseness.
   - CBOR features without a JSON counterpart (byte strings, tags,
     undefined, simple values, non-text map keys) yield an error. *)

open Printf
module SE = EndianString.BigEndian_unsafe

(* Internal control-flow exceptions. Neither escapes [from_string]:
   [Decode_error] is caught and surfaced as [Result.Error]; [Break]
   terminates an indefinite-length sequence and is consumed by the
   nearest [extract_list] / [extract_text]. *)
exception Decode_error of string
exception Break

type json = Yojson.Basic.t

let fail fmt = ksprintf (fun s -> raise (Decode_error s)) fmt

let need (s, i) n =
  if n > String.length s || !i + n > String.length s then
    fail "truncated: len %d pos %d need %d" (String.length s) !i n;
  let j = !i in
  i := !i + n;
  j

let get_byte ((s, _) as r) = int_of_char s.[need r 1]
let get_n ((s, _) as r) n f = f s (need r n)
let get_s ((s, _) as r) n = String.sub s (need r n) n

let get_additional byte1 = byte1 land 0b11111
let is_indefinite byte1 = get_additional byte1 = 31

let int64_max_int = Int64.of_int max_int
let two_min_int32 = 2 * Int32.to_int Int32.min_int

let extract_number byte1 r =
  match get_additional byte1 with
  | n when n < 24 -> n
  | 24 -> get_byte r
  | 25 -> get_n r 2 SE.get_uint16
  | 26 ->
      let n = Int32.to_int (get_n r 4 SE.get_int32) in
      if n < 0 then n - two_min_int32 else n
  | 27 ->
      let n = get_n r 8 SE.get_int64 in
      if n > int64_max_int || n < 0L then fail "extract_number: %Lu" n;
      Int64.to_int n
  | n -> fail "bad additional %d" n

let get_float16 s i =
  let half = (Char.code s.[i] lsl 8) + Char.code s.[i + 1] in
  let mant = half land 0x3ff in
  let value =
    match (half lsr 10) land 0x1f with
    | 31 when mant = 0 -> infinity
    | 31 -> nan
    | 0 -> ldexp (float mant) ~-24
    | exp -> ldexp (float (mant + 1024)) (exp - 25)
  in
  if half land 0x8000 = 0 then value else ~-.value

let extract_list byte1 r f =
  if is_indefinite byte1 then
    let l = ref [] in
    try
      while true do
        l := f r :: !l
      done;
      assert false
    with Break -> List.rev !l
  else
    let n = extract_number byte1 r in
    Array.to_list (Array.init n (fun _ -> f r))

let rec extract r : json =
  let byte1 = get_byte r in
  match byte1 lsr 5 with
  | 0 -> `Int (extract_number byte1 r)
  | 1 -> `Int (-1 - extract_number byte1 r)
  | 2 -> fail "byte string is not representable as JSON"
  | 3 -> `String (extract_text byte1 r)
  | 4 -> `List (extract_list byte1 r extract)
  | 5 -> `Assoc (extract_list byte1 r extract_field)
  | 6 -> fail "tag is not representable as JSON"
  | 7 -> (
      match get_additional byte1 with
      | n when n < 20 -> fail "simple value (%d) is not representable as JSON" n
      | 20 -> `Bool false
      | 21 -> `Bool true
      | 22 -> `Null
      | 23 -> fail "undefined value is not representable as JSON"
      | 24 ->
          fail "simple value (%d) is not representable as JSON" (get_byte r)
      | 25 -> `Float (get_n r 2 get_float16)
      | 26 -> `Float (get_n r 4 SE.get_float)
      | 27 -> `Float (get_n r 8 SE.get_double)
      | 31 -> raise Break
      | a -> fail "extract: (7,%d)" a)
  | _ -> assert false

and extract_text byte1 r : string =
  if is_indefinite byte1 then
    let b = Buffer.create 10 in
    try
      while true do
        Buffer.add_string b
          (match extract r with
          | `String s -> s
          | _ -> fail "indefinite text string chunk is not a text string")
      done;
      assert false
    with Break -> Buffer.contents b
  else
    let n = extract_number byte1 r in
    get_s r n

(* Reads a map field directly into [(string * json)], bypassing the
   [`String s] polyvariant box that [extract] would produce for a text
   key. [Break] from the head byte propagates out for the enclosing
   [extract_list] to finalize an indefinite-length map. *)
and extract_field r : string * json =
  let byte1 = get_byte r in
  match byte1 lsr 5 with
  | 7 when get_additional byte1 = 31 -> raise Break
  | 3 ->
      let s = extract_text byte1 r in
      let v =
        try extract r with Break -> fail "extract_field: unexpected break"
      in
      (s, v)
  | _ -> fail "CBOR map key is not a text string"

let from_string (s : string) : (json, string) result =
  let i = ref 0 in
  match extract (s, i) with
  | exception Decode_error msg -> Error msg
  | exception Break -> Error "decode: unexpected break"
  | x ->
      if !i = String.length s then Ok x
      else
        Error
          (sprintf "decode: extra data: len %d pos %d" (String.length s) !i)
