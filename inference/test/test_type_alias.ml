open Inference
open Protocols

let ty (s : string) : Ty.t = Ty.parse s

let typedef (alias : string) (renames : string) : Typedef.t =
  { alias = ty alias; ty = ty renames; location = Stage0.Location.empty }

let db (l : (string * string) list) : Type_alias.t =
  List.fold_left (fun db (a, r) -> Type_alias.add (typedef a r) db) Type_alias.empty l

let check (name : string) (expected : string) (given : Ty.t) =
  Alcotest.(check string) name expected (Ty.to_string given)

let resolves (name : string) (aliases : (string * string) list) (input : string)
    (expected : string) =
  ( name,
    `Quick,
    fun () -> check name expected (Type_alias.resolve (ty input) (db aliases)) )

let alias_tests =
  [
    resolves "a name that renames nothing is itself" [] "int" "int";
    resolves "an alias resolves to what it renames"
      [ ("LatLong", "struct latLong") ]
      "LatLong" "struct latLong";
    resolves "a name no alias mentions is left alone"
      [ ("LatLong", "struct latLong") ]
      "Other" "Other";
    (* An alias is registered unqualified, and a use may write a qualifier
       in front of it, so the two spellings have to reach the same type. *)
    resolves "a qualified use of an alias keeps the qualifier"
      [ ("LatLong", "struct latLong") ]
      "const LatLong" "const struct latLong";
    (* Only the alias is replaced: a qualifier the alias itself carries is
       not something the use asked for. *)
    resolves "an unqualified use of an alias gains no qualifier"
      [ ("LatLong", "struct latLong") ]
      "LatLong" "struct latLong";
  ]

let composite_tests =
  [
    (* The alias appears below the outermost level, where a lookup of the
       whole type finds nothing. *)
    resolves "a pointer to an alias points at what it renames"
      [ ("LatLong", "struct latLong") ]
      "LatLong *" "struct latLong *";
    resolves "an array of an alias holds what it renames"
      [ ("LatLong", "struct latLong") ]
      "LatLong [4]" "struct latLong[4]";
    resolves "a pointer to a qualified alias keeps the qualifier"
      [ ("LatLong", "struct latLong") ]
      "const LatLong *" "const struct latLong *";
    resolves "a pointer to a pointer to an alias descends twice"
      [ ("LatLong", "struct latLong") ]
      "LatLong **" "struct latLong * *";
    resolves "a composite of no alias is unchanged" [] "int *" "int *";
  ]

let chain_tests =
  [
    (* A chain declared in order: each alias is resolved against the ones
       already known. *)
    resolves "a chain declared in order collapses"
      [ ("A", "struct x"); ("B", "A"); ("C", "B") ]
      "C" "struct x";
    (* The same chain declared backwards, which one pass over the
       declarations in order cannot collapse: adding an alias has to
       rewrite the ones already known against it. *)
    resolves "a chain declared backwards collapses"
      [ ("C", "B"); ("B", "A"); ("A", "struct x") ]
      "C" "struct x";
    resolves "a backwards chain collapses below a pointer"
      [ ("C", "B"); ("B", "A"); ("A", "struct x") ]
      "C *" "struct x *";
    resolves "the middle of a backwards chain collapses too"
      [ ("C", "B"); ("B", "A"); ("A", "struct x") ]
      "B" "struct x";
  ]

(* Many spellings name one scalar, so an alias whose own name is one of
   them would be keyed by that scalar and rename every other spelling of it
   along with itself. *)
let shared_spelling_tests =
  [
    resolves "an alias named like a scalar renames nothing else"
      [ ("size_t", "unsigned long") ]
      "unsigned long long" "unsigned long long";
    resolves "an alias named like a scalar renames nothing below a pointer"
      [ ("int8_t", "signed char") ]
      "char *" "char *";
    resolves "an alias over a scalar still renames what it was given"
      [ ("cell_t", "unsigned long") ]
      "cell_t" "unsigned long";
  ]

let all_tests =
  [
    ("aliases", alias_tests);
    ("composites", composite_tests);
    ("chains", chain_tests);
    ("shared spellings", shared_spelling_tests);
  ]

let () = Alcotest.run "Type_alias" all_tests
