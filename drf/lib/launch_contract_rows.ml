type family = Gla | Wkv | Wkv7

type t = {
  row_id : string;
  family : family;
  manifest_kernel : string;
  parsed_kernel : string;
  template_arg : string;
  template_param : string;
  template_value : int;
}

let gla ~(row_id : string) ~(head_size : int) : t =
  {
    row_id;
    family = Gla;
    manifest_kernel = "gated_linear_attn_f32<" ^ string_of_int head_size ^ ">";
    parsed_kernel = "gated_linear_attn_f32";
    template_arg = string_of_int head_size;
    template_param = "HEAD_SIZE";
    template_value = head_size;
  }

let wkv ~(row_id : string) ~(template_arg : string) ~(block_size : int) : t =
  {
    row_id;
    family = Wkv;
    manifest_kernel = "rwkv_wkv_f32<" ^ template_arg ^ ">";
    parsed_kernel = "rwkv_wkv_f32";
    template_arg;
    template_param = "block_size";
    template_value = block_size;
  }

let wkv7 ~(row_id : string) ~(template_arg : string) ~(block_size : int) : t =
  {
    row_id;
    family = Wkv7;
    manifest_kernel = "rwkv_wkv7_f32<" ^ template_arg ^ ">";
    parsed_kernel = "rwkv_wkv7_f32";
    template_arg;
    template_param = "block_size";
    template_value = block_size;
  }

let all : t list =
  [
    gla ~row_id:"L072" ~head_size:64;
    gla ~row_id:"L073" ~head_size:128;
    wkv ~row_id:"L143" ~template_arg:"CUDA_WKV_BLOCK_SIZE" ~block_size:64;
    wkv ~row_id:"L144" ~template_arg:"CUDA_WKV_BLOCK_SIZE * 2" ~block_size:128;
    wkv7 ~row_id:"L145" ~template_arg:"CUDA_WKV_BLOCK_SIZE" ~block_size:64;
    wkv7 ~row_id:"L146" ~template_arg:"CUDA_WKV_BLOCK_SIZE * 2" ~block_size:128;
  ]
