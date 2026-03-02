(* Container type for code generation flags *)
type t = {
  const_fold : bool;
  distinct_vars : bool;
  div_to_mult : bool;
  expand_device : bool;
  gen_params : bool;
  mod_gv_args : bool;
  simplify_kernel : bool;
  toml : bool;
  use_dummy_array : bool;
}

let make ~const_fold ~distinct_vars ~div_to_mult ~expand_device ~gen_params
    ~mod_gv_args ~racuda ~simplify_kernel ~toml ~use_dummy_array : t =
  {
    (* RaCUDA option enables all code generation flags *)
    const_fold = const_fold || racuda;
    distinct_vars = distinct_vars || racuda;
    div_to_mult = div_to_mult || racuda;
    expand_device = expand_device || racuda;
    gen_params = gen_params || racuda;
    mod_gv_args = mod_gv_args || racuda;
    simplify_kernel = simplify_kernel || racuda;
    toml;
    use_dummy_array = use_dummy_array || racuda;
  }
