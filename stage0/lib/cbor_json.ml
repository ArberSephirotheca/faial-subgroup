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

(* The reader is the source string [s] and a mutable cursor [i] passed
   side-by-side. Threading them as a tuple (or wrapping in a record)
   would cost one tuple/record load per primitive read; passing the two
   words directly avoids that on the hot path. *)
let need s i n =
  if n > String.length s || !i + n > String.length s then
    fail "truncated: len %d pos %d need %d" (String.length s) !i n;
  let j = !i in
  i := !i + n;
  j

(* [need] has already validated that the read fits in the buffer, so
   the per-byte and per-substring bounds check that [s.[]] / [String.sub]
   would do is redundant; [SE]'s [_unsafe] variants skip the same check. *)
let get_byte s i = Char.code (String.unsafe_get s (need s i 1))
let get_n s i n f = f s (need s i n)
let get_s s i n =
  let j = need s i n in
  let b = Bytes.create n in
  Bytes.unsafe_blit_string s j b 0 n;
  Bytes.unsafe_to_string b

let get_additional byte1 = byte1 land 0b11111
let is_indefinite byte1 = get_additional byte1 = 31

(* Open-addressing pool of canonical map-key strings, keyed by source
   bytes [s.[off..off+len-1]] without first allocating a new string.
   On a hit the existing canonical string is returned and the source
   range is never copied; on a miss we allocate once and insert.

   c-to-json's output is dominated by a small set of keys repeated
   millions of times ("kind", "id", "inner", "range", ...); sharing
   the canonical allocation across the resulting Yojson.Basic.t tree
   measurably reduces resident memory.

   The pool persists at module scope and is cleared by [from_string]
   on each decode. Open addressing with a power-of-two capacity, 50%
   max load factor; empty slots use [-1] in a parallel hash array
   so the empty string is a valid key without sentinel ambiguity. *)
module Key_pool = struct
  let initial_capacity = 256

  let hashes = ref (Array.make initial_capacity (-1))
  let keys = ref (Array.make initial_capacity "")
  let size = ref 0
  let mask = ref (initial_capacity - 1)

  let clear () =
    Array.fill !hashes 0 (Array.length !hashes) (-1);
    size := 0

  let hash_substr s off len =
    let h = ref 0 in
    for k = 0 to len - 1 do
      h := (!h * 31) + Char.code (String.unsafe_get s (off + k))
    done;
    !h land max_int

  let substr_eq s off len e =
    String.length e = len
    &&
    let rec loop k =
      k = len
      || (String.unsafe_get s (off + k) = String.unsafe_get e k
         && loop (k + 1))
    in
    loop 0

  let copy_substr s off len =
    let b = Bytes.create len in
    Bytes.unsafe_blit_string s off b 0 len;
    Bytes.unsafe_to_string b

  let rec insert h k =
    let m = !mask in
    let buckets_h = !hashes in
    let buckets_k = !keys in
    let rec probe idx =
      if buckets_h.(idx) = -1 then begin
        buckets_h.(idx) <- h;
        buckets_k.(idx) <- k;
        incr size
      end
      else probe ((idx + 1) land m)
    in
    probe (h land m)

  and resize () =
    let old_h = !hashes in
    let old_k = !keys in
    let new_cap = Array.length old_h * 2 in
    hashes := Array.make new_cap (-1);
    keys := Array.make new_cap "";
    mask := new_cap - 1;
    size := 0;
    for idx = 0 to Array.length old_h - 1 do
      let h = old_h.(idx) in
      if h >= 0 then insert h old_k.(idx)
    done

  let intern s off len =
    let h = hash_substr s off len in
    let m = !mask in
    let buckets_h = !hashes in
    let buckets_k = !keys in
    let rec probe idx =
      let entry_h = buckets_h.(idx) in
      if entry_h = -1 then begin
        let canon = copy_substr s off len in
        buckets_h.(idx) <- h;
        buckets_k.(idx) <- canon;
        incr size;
        if !size * 2 > Array.length buckets_h then resize ();
        canon
      end
      else if entry_h = h && substr_eq s off len buckets_k.(idx) then
        buckets_k.(idx)
      else probe ((idx + 1) land m)
    in
    probe (h land m)
end

let int64_max_int = Int64.of_int max_int
let two_min_int32 = 2 * Int32.to_int Int32.min_int

let extract_number byte1 s i =
  match get_additional byte1 with
  | n when n < 24 -> n
  | 24 -> get_byte s i
  | 25 -> get_n s i 2 SE.get_uint16
  | 26 ->
      let n = Int32.to_int (get_n s i 4 SE.get_int32) in
      if n < 0 then n - two_min_int32 else n
  | 27 ->
      let n = get_n s i 8 SE.get_int64 in
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

(* Pre-allocated [`Int n] boxes for small non-negative values. c-to-json
   typically emits ~1.6M [`Int n] per kernel — column numbers, line
   numbers, AST tags, small literals — and the vast majority fall in
   [0, int_cache_size). Returning a shared box on a hit removes one
   3-word polyvariant allocation per int decoded on the hot path; the
   cache itself is ~32 KB resident. *)
let int_cache_size = 1024
let int_cache : json array =
  Array.init int_cache_size (fun n -> `Int n)

let make_int n : json =
  if n >= 0 && n < int_cache_size then Array.unsafe_get int_cache n
  else `Int n

(* Arrays and maps are read by [extract_array] / [extract_map] — separate
   specializations rather than passing the per-element extractor as a
   parameter. This removes one indirect call per element and lets the
   compiler inline the body of [extract] / [extract_field] into the
   loop. *)
(* Arrays and maps are read by [extract_array] / [extract_map] — separate
   specializations rather than passing the per-element extractor as a
   parameter. This removes one indirect call per element and lets the
   compiler inline the body of [extract] / [extract_field] into the
   loop. *)
let rec extract s i : json =
  let byte1 = get_byte s i in
  match byte1 lsr 5 with
  | 0 -> make_int (extract_number byte1 s i)
  | 1 -> `Int (-1 - extract_number byte1 s i)
  | 2 -> fail "byte string is not representable as JSON"
  | 3 -> `String (extract_text byte1 s i)
  | 4 -> `List (extract_array byte1 s i)
  | 5 -> `Assoc (extract_map byte1 s i)
  | 6 -> fail "tag is not representable as JSON"
  | 7 -> (
      match get_additional byte1 with
      | n when n < 20 -> fail "simple value (%d) is not representable as JSON" n
      | 20 -> `Bool false
      | 21 -> `Bool true
      | 22 -> `Null
      | 23 -> fail "undefined value is not representable as JSON"
      | 24 ->
          fail "simple value (%d) is not representable as JSON" (get_byte s i)
      | 25 -> `Float (get_n s i 2 get_float16)
      | 26 -> `Float (get_n s i 4 SE.get_float)
      | 27 -> `Float (get_n s i 8 SE.get_double)
      | 31 -> raise Break
      | a -> fail "extract: (7,%d)" a)
  | _ -> assert false

and extract_array byte1 s i : json list =
  if is_indefinite byte1 then
    let l = ref [] in
    try
      while true do
        l := extract s i :: !l
      done;
      assert false
    with Break -> List.rev !l
  else
    let n = extract_number byte1 s i in
    Array.to_list (Array.init n (fun _ -> extract s i))

and extract_map byte1 s i : (string * json) list =
  if is_indefinite byte1 then
    let l = ref [] in
    try
      while true do
        l := extract_field s i :: !l
      done;
      assert false
    with Break -> List.rev !l
  else
    let n = extract_number byte1 s i in
    Array.to_list (Array.init n (fun _ -> extract_field s i))

and extract_text byte1 s i : string =
  if is_indefinite byte1 then
    let b = Buffer.create 10 in
    try
      while true do
        Buffer.add_string b
          (match extract s i with
          | `String chunk -> chunk
          | _ -> fail "indefinite text string chunk is not a text string")
      done;
      assert false
    with Break -> Buffer.contents b
  else
    let n = extract_number byte1 s i in
    get_s s i n

(* Reads a map field directly into [(string * json)], bypassing the
   [`String s] polyvariant box that [extract] would produce for a text
   key. Definite-length keys are interned by hashing the source bytes
   in place via [Key_pool.intern], so a repeated key never allocates
   a fresh string. Indefinite-length keys (rare) are built by
   [extract_text] and then interned post-hoc. [Break] from the head
   byte propagates out for the enclosing [extract_map] to finalize an
   indefinite-length map. *)
and extract_field s i : string * json =
  let byte1 = get_byte s i in
  match byte1 lsr 5 with
  | 7 when get_additional byte1 = 31 -> raise Break
  | 3 ->
      let k =
        if is_indefinite byte1 then
          let str = extract_text byte1 s i in
          Key_pool.intern str 0 (String.length str)
        else
          let n = extract_number byte1 s i in
          let off = need s i n in
          Key_pool.intern s off n
      in
      let v =
        try extract s i with Break -> fail "extract_field: unexpected break"
      in
      (k, v)
  | _ -> fail "CBOR map key is not a text string"

let from_string (s : string) : (json, string) result =
  Key_pool.clear ();
  let i = ref 0 in
  match extract s i with
  | exception Decode_error msg -> Error msg
  | exception Break -> Error "decode: unexpected break"
  | x ->
      if !i = String.length s then Ok x
      else
        Error
          (sprintf "decode: extra data: len %d pos %d" (String.length s) !i)
