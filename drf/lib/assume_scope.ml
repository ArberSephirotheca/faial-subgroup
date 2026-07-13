(* Splits the [--assume "[KERNEL:]BEXP"] argument into its optional
   kernel-name scope and the remaining boolean-expression text.

   The [KERNEL:] prefix targets a single kernel by name; without it the
   clause applies to every kernel whose params cover its free
   variables. The prefix is recognised as a scope only when it looks
   like a kernel identifier: letters, digits, underscore, and [@]. The
   [@] appears in the uniquified names of the launch pseudo-kernels
   that [--assume-launch] synthesises (for example
   [rope_multi@rope_457_2]), so it must count as an identifier
   character for those kernels to be addressable by name. A [:] inside
   the BEXP itself never triggers scoping: the bexp grammar has no [:]
   token, so a prefix that is not a bare identifier is left as part of
   the expression. *)
let looks_like_ident (s : string) : bool =
  s <> ""
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_' || c = '@')
       s

(* Returns [(Some kernel, bexp_text)] when [s] is [KERNEL:BEXP] with an
   identifier-shaped [KERNEL]; otherwise [(None, s)]. *)
let split (s : string) : string option * string =
  match String.index_opt s ':' with
  | None -> (None, s)
  | Some i ->
      let prefix = String.sub s 0 i |> String.trim in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      if looks_like_ident prefix then (Some prefix, rest) else (None, s)
