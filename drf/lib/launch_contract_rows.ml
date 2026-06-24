type family = Gla

type t = {
  row_id : string;
  family : family;
  manifest_kernel : string;
  parsed_kernel : string;
  head_size : int;
}

let gla ~(row_id : string) ~(head_size : int) : t =
  {
    row_id;
    family = Gla;
    manifest_kernel = "gated_linear_attn_f32<" ^ string_of_int head_size ^ ">";
    parsed_kernel = "gated_linear_attn_f32";
    head_size;
  }

let all : t list =
  [ gla ~row_id:"L072" ~head_size:64; gla ~row_id:"L073" ~head_size:128 ]
