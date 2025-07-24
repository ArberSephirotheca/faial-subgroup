open Stage0
open Protocols
module StackTrace = Stack_trace

type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

(* Monadic let *)
let ( let* ) = Result.bind

(* Monadic pipe *)
let ( >>= ) = Result.bind

(** {1 Scalar Module}

    This module defines scalar types as used in WGSL (WebGPU Shading Language).
    WGSL supports multiple scalar types, including integers, floats, and
    booleans, which are fundamental to shader programming. *)
module ScalarKind = struct
  (** Type definition for scalar kinds. *)
  type t =
    | Sint  (** Signed integer type. *)
    | Uint  (** Unsigned integer type. *)
    | Float  (** Floating-point type. *)
    | Bool  (** Boolean type. *)
    | AbstractInt  (** Abstract integer type (not bound to a specific size). *)
    | AbstractFloat
        (** Abstract floating-point type (not bound to a specific size). *)

  (** Checks if a scalar kind is an integer type.
      @param k The scalar kind to check.
      @return
        [true] if the scalar kind is [Sint], [Uint], or [AbstractInt], otherwise
        [false]. *)
  let is_int : t -> bool = function
    | Sint | Uint | AbstractInt -> true
    | Float | Bool | AbstractFloat -> false

  (** Checks if the given scalar kind is a boolean.
      @param k The scalar kind to check.
      @return [true] if the scalar kind is [Bool], otherwise [false]. *)
  let is_bool (k : t) : bool = k = Bool

  (** Parses a JSON value into a scalar kind.
      @param j The JSON value to parse.
      @return
        A result containing the parsed scalar kind on success or an error
        message on failure. *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* n = cast_string j in
    match n with
    | "Sint" -> Ok Sint
    | "Uint" -> Ok Uint
    | "Float" -> Ok Float
    | "Bool" -> Ok Bool
    | "AbstractInt" -> Ok AbstractInt
    | "AbstractFloat" -> Ok AbstractFloat
    | _ -> root_cause ("ScalarKind.parse: unknown kind: " ^ n) j

  (** Converts a scalar kind to its corresponding string representation.
      @param k The scalar kind to convert.
      @return The string representation of the scalar kind. *)
  let to_string : t -> string = function
    | Sint -> "i"
    | Uint -> "u"
    | Float -> "f"
    | Bool -> "bool"
    | AbstractInt -> "abstract i"
    | AbstractFloat -> "abstract f"

  (** Checks if a scalar kind is an unsigned integer.
      @param x The scalar kind to check.
      @return [true] if the scalar kind is [Uint], otherwise [false]. *)
  let is_unsigned (x : t) : bool = x = Uint
end

(** {1 VectorSize Module}
    This module provides utilities to handle vector sizes in WGSL (WebGPU
    Shading Language). In WGSL, vectors can have different sizes (2, 3, or 4
    components), corresponding to typical constructs in shader programming, such
    as 2D, 3D, or 4D vectors.

    This module defines a type to represent these sizes, along with utility
    functions for converting between representations, accessing components, and
    parsing from JSON. *)
module Scalar = struct
  (** {2 Types} *)

  type t = { kind : ScalarKind.t; width : int }
  (** The type representing a scalar, which includes a kind and width. *)

  (** {2 Predefined Scalars} *)

  (** [u32] is an unsigned 32-bit integer scalar. *)
  let u32 : t = { kind = ScalarKind.Uint; width = 4 }

  (** [u64] is an unsigned 64-bit integer scalar. *)
  let u64 : t = { kind = ScalarKind.Uint; width = 8 }

  (** [i32] is a signed 32-bit integer scalar. *)
  let i32 : t = { kind = ScalarKind.Sint; width = 4 }

  (** [i64] is a signed 64-bit integer scalar. *)
  let i64 : t = { kind = ScalarKind.Sint; width = 8 }

  (** [f32] is a 32-bit floating-point scalar. This is common in WGSL for
      representing fractional values with limited precision. *)
  let f32 : t = { kind = ScalarKind.Float; width = 4 }

  (** [f64] is a 64-bit floating-point scalar, offering higher precision
      compared to [f32]. *)
  let f64 : t = { kind = ScalarKind.Float; width = 8 }

  (** [bool] is a boolean scalar, typically used for conditional logic. WGSL
      booleans are 4 bytes wide (32 bits). *)
  let bool : t = { kind = ScalarKind.Bool; width = 4 }

  (** [int] is an abstract integer scalar, used when the integer width is not
      explicitly defined. *)
  let int : t = { kind = ScalarKind.AbstractInt; width = 8 }

  (** [float] is an abstract floating-point scalar, used when the width of the
      float is unspecified. *)
  let float : t = { kind = ScalarKind.AbstractFloat; width = 8 }

  (** {2 Constructors} *)

  (** [make_32 kind] creates a 32-bit scalar of the given [kind]. This is useful
      for types like [i32] and [f32]. *)
  let make_32 (kind : ScalarKind.t) : t = { kind; width = 4 }

  (** [make_64 kind] creates a 64-bit scalar of the given [kind]. Use this for
      constructing larger precision types like [i64] and [f64]. *)
  let make_64 (kind : ScalarKind.t) : t = { kind; width = 8 }

  (** {2 Predicates} *)

  (** [is_int s] checks if the scalar [s] is of an integer type (either signed
      or unsigned). *)
  let is_int (s : t) : bool = ScalarKind.is_int s.kind

  (** [is_bool s] checks if the scalar [s] is a boolean. Boolean scalars are
      often used in control flow in WGSL shaders. *)
  let is_bool (s : t) : bool = ScalarKind.is_bool s.kind

  (** [is_unsigned s] checks if the scalar [s] is an unsigned integer. *)
  let is_unsigned (s : t) : bool = ScalarKind.is_unsigned s.kind

  (** {2 Conversion} *)

  (** [to_string s] converts the scalar [s] to a string representation. The
      format includes the scalar kind and its bit width. For example, an [i32]
      scalar will be rendered as "Sint32". *)
  let to_string (s : t) : string =
    if s.kind = Bool then "bool"
    else ScalarKind.to_string s.kind ^ string_of_int (8 * s.width)

  (** {2 Parsing} *)

  (** [parse j] parses a JSON object [j] into a scalar [t].

      The expected JSON structure is:
      {[
        {
          "kind": "Uint" | "Sint" | "Float" | "Bool" | "AbstractInt" | "AbstractFloat",
          "width": <int>
        }
      ]}

      This function returns [Ok t] if the parsing succeeds, or an error if the
      JSON format is invalid. *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = with_field "kind" ScalarKind.parse o in
    let* width = with_field "width" cast_int o in
    Ok { kind; width }
end

(** {1 StorageAccess Module}

    This module defines the type of storage access in WGSL (WebGPU Shading
    Language) and provides utilities to convert these types to strings and parse
    them from JSON. In WGSL, storage access controls how buffers and textures
    are accessed in shaders.

    Example:
    - A `read_write` buffer in WGSL would correspond to `ReadWrite` here.
    - A texture with `readonly` access corresponds to `ReadOnly`.

    @see <https://www.w3.org/TR/WGSL/#storage-texture-types>
      WGSL Storage Texture Types *)
module VectorSize = struct
  (** {2 Types} *)

  (** The type [t] represents the possible sizes of vectors in WGSL. *)
  type t =
    | Bi  (** Represents a 2D vector (e.g., `vec2` in WGSL). *)
    | Tri  (** Represents a 3D vector (e.g., `vec3` in WGSL). *)
    | Quad  (** Represents a 4D vector (e.g., `vec4` in WGSL). *)

  (** {2 Functions} *)

  (** [to_int t] converts the vector size [t] to its integer representation.
      - [Bi] returns 2.
      - [Tri] returns 3.
      - [Quad] returns 4.

      Example:
      {[
        let size = VectorSize.to_int Tri (* returns 3 *)
      ]} *)
  let to_int : t -> int = function Bi -> 2 | Tri -> 3 | Quad -> 4

  (** [from_int n] converts an integer [n] to a [VectorSize.t] option. If the
      integer is not 2, 3, or 4, returns [None].

      Example:
      {[
        match VectorSize.from_int 3 with
        | Some size -> (* Use size *)
        | None -> failwith "Invalid size"
      ]} *)
  let from_int (n : int) : t option =
    match n with 2 -> Some Bi | 3 -> Some Tri | 4 -> Some Quad | _ -> None

  (** [components t] returns the list of component names for the given vector
      size [t]. The components follow WGSL conventions with
      ["x"; "y"; "z"; "w"].

      Example:
      {[
        VectorSize.components Quad (* returns ["x"; "y"; "z"; "w"] *)
      ]} *)
  let components : t -> string list = function
    | Bi -> [ "x"; "y" ]
    | Tri -> [ "x"; "y"; "z" ]
    | Quad -> [ "x"; "y"; "z"; "w" ]

  (** [nth_opt n ty] returns the [n]-th component of the vector size [ty], if it
      exists. Uses [List.nth_opt] to safely access components by index.

      Example:
      {[
        VectorSize.nth_opt 2 Tri (* returns Some "z" *)
      ]} *)
  let nth_opt (n : int) (ty : t) : string option =
    List.nth_opt (components ty) n

  (** [to_string s] converts a [VectorSize.t] value to its string
      representation. This is useful for logging or debugging purposes.

      Example:
      {[
        let str = VectorSize.to_string Tri (* returns "3" *)
      ]} *)
  let to_string (s : t) : string = to_int s |> string_of_int

  (** [parse j] parses a JSON value [j] into a [VectorSize.t] value. If the JSON
      value is not a valid vector size, returns an error.

      Example:
      {[
        let json = `Int 3 in
        match VectorSize.parse json with
        | Ok size -> (* Use size *)
        | Error msg -> print_endline msg
      ]} *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* n = cast_int j in
    match from_int n with
    | Some n -> Ok n
    | None -> root_cause "VectorSize.parse: invalid JSON" j
end

(** {1 AddressSpace Module}
    This module defines the various address spaces in WGSL (WebGPU Shading
    Language), which are used to specify where variables are stored and how they
    are accessed during the execution of shaders. The types align with WGSL's
    memory model concepts. *)
module StorageAccess = struct
  (** {2 Types}

      [t] represents the different kinds of storage access that can exist in
      WGSL. It corresponds to the possible access qualifiers for storage buffers
      or textures. *)
  type t =
    | ReadWrite  (** Represents 'read_write' access in WGSL. *)
    | ReadOnly  (** Represents 'read' access in WGSL. *)
    | WriteOnly  (** Represents 'write' access in WGSL. *)

  (** Converts a [StorageAccess.t] value into a string that corresponds to the
      WGSL access qualifier.

      @param access The [StorageAccess.t] value to convert.
      @return A string representation of the access mode.

      Example:
      {[
        let mode = StorageAccess.ReadWrite in
        let str = StorageAccess.to_string mode; (* "read_write" *)
      ]} *)
  let to_string : t -> string = function
    | ReadWrite -> "read_write"
    | ReadOnly -> "read"
    | WriteOnly -> "write"

  (** Parses a JSON object into a [StorageAccess.t] value.

      The JSON is expected to have two boolean fields: ["load"] and ["store"].
      - ["load"] indicates if the storage can be read.
      - ["store"] indicates if the storage can be written to.

      @param j A JSON object to parse.
      @return A result with either the parsed [StorageAccess.t] or an error.

      Example:
      {[
        let json = `Assoc [ ("load", `Bool true); ("store", `Bool false) ] in
        match StorageAccess.parse json with
        | Ok ReadOnly -> print_endline "Parsed as ReadOnly"
        | _ -> print_endline "Failed to parse"
      ]}

      This function is useful for loading WGSL shader configurations from JSON.
  *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* load = with_field "load" cast_bool o in
    let* store = with_field "store" cast_bool o in
    match (load, store) with
    | true, true -> Ok ReadWrite
    | true, false -> Ok ReadOnly
    | false, true -> Ok WriteOnly
    | false, false -> root_cause "StorageAccess.parse: false, false" j
end

module AddressSpace = struct
  (** {2 Types} *)

  (** The type [t] represents the different address spaces in WGSL. *)
  type t =
    | Function  (** A function-level variable, local to the shader function. *)
    | Private  (** A private variable, local to a single shader invocation. *)
    | WorkGroup
        (** A variable shared among all invocations within a workgroup. *)
    | Uniform  (** A variable accessible via the uniform address space. *)
    | Storage of StorageAccess.t
        (** A storage buffer with specific access permissions. *)
    | Handle  (** A resource handle, such as a texture or sampler. *)
    | PushConstant
        (** A push constant, which provides fast access to small amounts of
            data. *)

  (** {2 Functions} *)

  (** [to_string addr_space] converts the address space [addr_space] to its
      string representation in WGSL.

      @param addr_space The address space to convert.
      @return A string representation of the address space.

      {b Example:}
      {[
        let s = AddressSpace.to_string Function
        (* s is "function" *)
      ]} *)
  let to_string : t -> string = function
    | Function -> "function"
    | Private -> "private"
    | WorkGroup -> "workgroup"
    | Uniform -> "uniform"
    | Storage a -> "storage, " ^ StorageAccess.to_string a
    | Handle -> ""
    | PushConstant -> "push_constant"

  (** [parse j] parses a JSON representation into an address space value [t].

      @param j The JSON value to parse.
      @return
        A result containing the parsed address space or an error if parsing
        fails.

      {b Example:}
      {[
        let json = `Assoc [ ("kind", `String "Function") ] in
        match AddressSpace.parse json with
        | Ok addr_space -> print_endline (AddressSpace.to_string addr_space)
        | Error e -> print_endline ("Error: " ^ e)
        (* Output: function *)
      ]}

      This function is useful for interpreting WGSL metadata stored in JSON
      format. *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Function" -> Ok Function
    | "Private" -> Ok Private
    | "WorkGroup" -> Ok WorkGroup
    | "Uniform" -> Ok Uniform
    | "Storage" ->
        let* a = with_field "access" StorageAccess.parse o in
        Ok (Storage a)
    | "Handle" -> Ok Handle
    | "PushConstant" -> Ok PushConstant
    | _ -> root_cause ("Unknown kind:" ^ kind) j
end

module ImageDimension = struct
  type t = D1 | D2 | D3 | Cube

  let is_1d : t -> bool = function D1 -> true | _ -> false

  let parse (j : json) : t j_result =
    let open Rjson in
    let* data = cast_string j in
    match data with
    | "D1" -> Ok D1
    | "D2" -> Ok D2
    | "D3" -> Ok D3
    | "Cube" -> Ok Cube
    | _ -> root_cause ("ImageDimension.parse: unknown: " ^ data) j

  let to_string : t -> string = function
    | D1 -> "1d"
    | D2 -> "2d"
    | D3 -> "3d"
    | Cube -> "cube"
end

module StorageFormat = struct
  type t =
    | R8Unorm
    | R8Snorm
    | R8Uint
    | R8Sint
    | R16Uint
    | R16Sint
    | R16Float
    | Rg8Unorm
    | Rg8Snorm
    | Rg8Uint
    | Rg8Sint
    | R32Uint
    | R32Sint
    | R32Float
    | Rg16Uint
    | Rg16Sint
    | Rg16Float
    | Rgba8Unorm
    | Rgba8Snorm
    | Rgba8Uint
    | Rgba8Sint
    | Bgra8Unorm
    | Rgb10a2Uint
    | Rgb10a2Unorm
    | Rg11b10Float
    | Rg32Uint
    | Rg32Sint
    | Rg32Float
    | Rgba16Uint
    | Rgba16Sint
    | Rgba16Float
    | Rgba32Uint
    | Rgba32Sint
    | Rgba32Float
    | R16Unorm
    | R16Snorm
    | Rg16Unorm
    | Rg16Snorm
    | Rgba16Unorm
    | Rgba16Snorm

  let to_string : t -> string = function
    | R8Unorm -> "r8_unorm"
    | R8Snorm -> "r8_snorm"
    | R8Uint -> "r8_uint"
    | R8Sint -> "r8_sint"
    | R16Uint -> "r16_uint"
    | R16Sint -> "r16_sint"
    | R16Float -> "r16_float"
    | Rg8Unorm -> "rg8_unorm"
    | Rg8Snorm -> "rg8_snorm"
    | Rg8Uint -> "rg8_uint"
    | Rg8Sint -> "rg8_sint"
    | R32Uint -> "r32_uint"
    | R32Sint -> "r32_sint"
    | R32Float -> "r32_float"
    | Rg16Uint -> "rg16_uint"
    | Rg16Sint -> "rg16_sint"
    | Rg16Float -> "rg16_float"
    | Rgba8Unorm -> "rgba8_unorm"
    | Rgba8Snorm -> "rgba8_snorm"
    | Rgba8Uint -> "rgba8_uint"
    | Rgba8Sint -> "rgba8_sint"
    | Bgra8Unorm -> "bgra8_unorm"
    | Rgb10a2Uint -> "rgb10a2_uint"
    | Rgb10a2Unorm -> "rgb10a2_unorm"
    | Rg11b10Float -> "rg11b10_float"
    | Rg32Uint -> "rg32_uint"
    | Rg32Sint -> "rg32_sint"
    | Rg32Float -> "rg32_float"
    | Rgba16Uint -> "rgba16_uint"
    | Rgba16Sint -> "rgba16_sint"
    | Rgba16Float -> "rgba16_float"
    | Rgba32Uint -> "rgba32_uint"
    | Rgba32Sint -> "rgba32_sint"
    | Rgba32Float -> "rgba32_float"
    | R16Unorm -> "r16_unorm"
    | R16Snorm -> "r16_snorm"
    | Rg16Unorm -> "rg16_unorm"
    | Rg16Snorm -> "rg16_snorm"
    | Rgba16Unorm -> "rgba16_unorm"
    | Rgba16Snorm -> "rgba16_snorm"

  let to_kind : t -> ScalarKind.t = function
    | R8Unorm | R8Snorm | R16Float | Rg8Unorm | Rg8Snorm | R32Float | Rg16Float
    | Rgba8Unorm | Rgba8Snorm | Bgra8Unorm | Rgb10a2Unorm | Rg11b10Float
    | Rg32Float | Rgba16Float | Rgba32Float | R16Unorm | R16Snorm | Rg16Unorm
    | Rg16Snorm | Rgba16Unorm | Rgba16Snorm ->
        Float
    | R8Uint | R16Uint | Rg8Uint | R32Uint | Rg16Uint | Rgba8Uint | Rgb10a2Uint
    | Rg32Uint | Rgba16Uint | Rgba32Uint ->
        Uint
    | R8Sint | R16Sint | Rg8Sint | R32Sint | Rg16Sint | Rgba8Sint | Rg32Sint
    | Rgba16Sint | Rgba32Sint ->
        Sint

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "R8Unorm" -> Ok R8Unorm
    | "R8Snorm" -> Ok R8Snorm
    | "R8Uint" -> Ok R8Uint
    | "R8Sint" -> Ok R8Sint
    | "R16Uint" -> Ok R16Uint
    | "R16Sint" -> Ok R16Sint
    | "R16Float" -> Ok R16Float
    | "Rg8Unorm" -> Ok Rg8Unorm
    | "Rg8Snorm" -> Ok Rg8Snorm
    | "Rg8Uint" -> Ok Rg8Uint
    | "Rg8Sint" -> Ok Rg8Sint
    | "R32Uint" -> Ok R32Uint
    | "R32Sint" -> Ok R32Sint
    | "R32Float" -> Ok R32Float
    | "Rg16Uint" -> Ok Rg16Uint
    | "Rg16Sint" -> Ok Rg16Sint
    | "Rg16Float" -> Ok Rg16Float
    | "Rgba8Unorm" -> Ok Rgba8Unorm
    | "Rgba8Snorm" -> Ok Rgba8Snorm
    | "Rgba8Uint" -> Ok Rgba8Uint
    | "Rgba8Sint" -> Ok Rgba8Sint
    | "Bgra8Unorm" -> Ok Bgra8Unorm
    | "Rgb10a2Uint" -> Ok Rgb10a2Uint
    | "Rgb10a2Unorm" -> Ok Rgb10a2Unorm
    | "Rg11b10Float" -> Ok Rg11b10Float
    | "Rg32Uint" -> Ok Rg32Uint
    | "Rg32Sint" -> Ok Rg32Sint
    | "Rg32Float" -> Ok Rg32Float
    | "Rgba16Uint" -> Ok Rgba16Uint
    | "Rgba16Sint" -> Ok Rgba16Sint
    | "Rgba16Float" -> Ok Rgba16Float
    | "Rgba32Uint" -> Ok Rgba32Uint
    | "Rgba32Sint" -> Ok Rgba32Sint
    | "Rgba32Float" -> Ok Rgba32Float
    | "R16Unorm" -> Ok R16Unorm
    | "R16Snorm" -> Ok R16Snorm
    | "Rg16Unorm" -> Ok Rg16Unorm
    | "Rg16Snorm" -> Ok Rg16Snorm
    | "Rgba16Unorm" -> Ok Rgba16Unorm
    | "Rgba16Snorm" -> Ok Rgba16Snorm
    | _ -> root_cause "StorageFormat" j
end

module ImageClass = struct
  type t =
    | Sampled of { kind : ScalarKind.t; multi : bool }
    | Depth of { multi : bool }
    | Storage of { format : StorageFormat.t; access : StorageAccess.t }

  let scalar_kind : t -> ScalarKind.t option = function
    | Sampled { kind; _ } -> Some kind
    | Depth _ -> None
    | Storage _ -> None

  let is_depth : t -> bool = function Depth _ -> true | _ -> false

  let multisampled : t -> bool = function
    | Sampled { multi; _ } | Depth { multi } -> multi
    | Storage _ -> false

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Sampled" ->
        let* kind = with_field "scalar_kind" ScalarKind.parse o in
        let* multi = with_field "multi" cast_bool o in
        Ok (Sampled { kind; multi })
    | "Depth" ->
        let* multi = with_field "multi" cast_bool o in
        Ok (Depth { multi })
    | "Storage" ->
        let* format = with_field "format" StorageFormat.parse o in
        let* access = with_field "access" StorageAccess.parse o in
        Ok (Storage { format; access })
    | _ -> root_cause "Unsupported kind" j

  let to_string : t -> string = function _ -> "ImageClass"
end

module Interpolation = struct
  type t = Perspective | Linear | Flat

  let to_string : t -> string = function
    | Perspective -> "perspective"
    | Linear -> "linear"
    | Flat -> "flat"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Perspective" -> Ok Perspective
    | "Linear" -> Ok Linear
    | "Flat" -> Ok Flat
    | _ -> root_cause "Interpolation" j
end

module Sampling = struct
  type t = Center | Centroid | Sample

  let to_string : t -> string = function
    | Center -> "center"
    | Centroid -> "centroid"
    | Sample -> "sample"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Center" -> Ok Center
    | "Centroid" -> Ok Centroid
    | "Sample" -> Ok Sample
    | _ -> root_cause "Sampling" j
end

module BuiltIn = struct
  type t =
    | PositionInvariant
    | PositionVariant
    | ViewIndex
    | InstanceIndex
    | VertexIndex
    | FragDepth
    | FrontFacing
    | PrimitiveIndex
    | SampleIndex
    | SampleMask
    | GlobalInvocationId
    | LocalInvocationId
    | LocalInvocationIndex
    | WorkGroupId
    | NumWorkGroups
    | NumSubgroups
    | SubgroupId
    | SubgroupSize
    | SubgroupInvocationId

  let is_concurrency_related : t -> bool = function
    | GlobalInvocationId | LocalInvocationId | LocalInvocationIndex
    | WorkGroupId | NumWorkGroups | NumSubgroups | SubgroupId | SubgroupSize
    | SubgroupInvocationId ->
        true
    | _ -> false

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "PositionInvariant" -> Ok PositionInvariant
    | "PositionVariant" -> Ok PositionVariant
    | "ViewIndex" -> Ok ViewIndex
    | "InstanceIndex" -> Ok InstanceIndex
    | "VertexIndex" -> Ok VertexIndex
    | "FragDepth" -> Ok FragDepth
    | "FrontFacing" -> Ok FrontFacing
    | "PrimitiveIndex" -> Ok PrimitiveIndex
    | "SampleIndex" -> Ok SampleIndex
    | "SampleMask" -> Ok SampleMask
    | "GlobalInvocationId" -> Ok GlobalInvocationId
    | "LocalInvocationId" -> Ok LocalInvocationId
    | "LocalInvocationIndex" -> Ok LocalInvocationIndex
    | "WorkGroupId" -> Ok WorkGroupId
    | "NumWorkGroups" -> Ok NumWorkGroups
    | "NumSubgroups" -> Ok NumSubgroups
    | "SubgroupId" -> Ok SubgroupId
    | "SubgroupSize" -> Ok SubgroupSize
    | "SubgroupInvocationId" -> Ok SubgroupInvocationId
    | _ -> root_cause ("BuiltIn.parse: Unknown kind: " ^ name) j

  let to_string : t -> string = function
    | VertexIndex -> "vertex_index"
    | InstanceIndex -> "instance_index"
    | PositionVariant -> "position"
    | PositionInvariant -> "position_invariant"
    | FrontFacing -> "front_facing"
    | FragDepth -> "frag_depth"
    | LocalInvocationId -> "local_invocation_id"
    | LocalInvocationIndex -> "local_invocation_index"
    | GlobalInvocationId -> "global_invocation_id"
    | WorkGroupId -> "workgroup_id"
    | NumWorkGroups -> "num_workgroups"
    | SampleIndex -> "sample_index"
    | SampleMask -> "sample_mask"
    | PrimitiveIndex -> "primitive_index"
    | ViewIndex -> "view_index"
    | NumSubgroups -> "num_subgroups"
    | SubgroupId -> "subgroup_id"
    | SubgroupSize -> "subgroup_size"
    | SubgroupInvocationId -> "subgroup_invocation_id"
end

module Binding = struct
  type t =
    | BuiltIn of BuiltIn.t
    | Location of {
        location : int;
        second_blend_source : bool;
        interpolation : Interpolation.t option;
        sampling : Sampling.t option;
      }

  let workgroup_id : t = BuiltIn BuiltIn.WorkGroupId
  let local_invocation_id : t = BuiltIn BuiltIn.LocalInvocationId
  let global_invocation_id : t = BuiltIn BuiltIn.GlobalInvocationId
  let num_workgroups : t = BuiltIn BuiltIn.NumWorkGroups

  let is_concurrency_related : t -> bool = function
    | BuiltIn b -> BuiltIn.is_concurrency_related b
    | _ -> false

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "BuiltIn" ->
        let* b = with_field "value" BuiltIn.parse o in
        Ok (BuiltIn b)
    | "Location" ->
        let* location = with_field "location" cast_int o in
        let* second_blend_source =
          with_field "second_blend_source" cast_bool o
        in
        let* interpolation =
          with_field "interpolation" (cast_option Interpolation.parse) o
        in
        let* sampling = with_field "sampling" (cast_option Sampling.parse) o in
        Ok (Location { location; second_blend_source; interpolation; sampling })
    | _ -> root_cause ("Unsupported kind: " ^ kind) j

  let to_string : t -> string = function
    | BuiltIn b -> "builtin(" ^ BuiltIn.to_string b ^ ")"
    | Location _ -> failwith "Binding.to_string: Location"
end

(** {1 ArraySize Module}

    This module defines and handles the size of arrays, which can be either a
    constant value or dynamic. It reflects WGSL’s capability of defining arrays
    with fixed or runtime-determined sizes.

    In WGSL, arrays are often sized either by compile-time constants or
    dynamically when using runtime-sized arrays, especially in buffer storage.
    This module provides tools for parsing and representing such constructs. *)

module ArraySize = struct
  (** {2 Types} *)

  (** The type [t] represents the size of an array in WGSL.
      - A [Constant] size is a positive integer representing a fixed size known
        at compile time.
      - [Dynamic] represents a size determined at runtime (like in runtime-sized
        arrays). *)
  type t =
    | Constant of int
        (** A constant array size, represented by a positive integer. *)
    | Dynamic  (** A dynamic size, where the size is determined at runtime. *)

  (** Converts an [ArraySize.t] value into its string representation.

      Example:
      {[
        ArraySize.to_string (Constant 4) (* returns "constant(4)" *)
          ArraySize.to_string Dynamic (* returns "dynamic" *)
      ]} *)
  let to_string : t -> string = function
    | Constant n -> Printf.sprintf "constant(%d)" n
    | Dynamic -> "dynamic"

  (** Converts the size to an optional integer. When the size is dynamic we get
      none, otherwise we get the constant size *)
  let to_int : t -> int option = function
    | Constant n -> Some n
    | Dynamic -> None

  (** Parses a JSON object into an [ArraySize.t] value.

      The input JSON object must have a "kind" field indicating the type of
      size, and, if it's a [Constant], it should also include a "value" field
      with a positive integer.

      Example JSON inputs:
      {[
        { "kind": "Constant", "value": 4 }  (* Parsed as Constant 4 *)
        { "kind": "Dynamic" }               (* Parsed as Dynamic *)
      ]}

      @param j The JSON object to parse.
      @return
        A result containing either the parsed [ArraySize.t] or an error message.
  *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Constant" ->
        let* value = with_field "value" cast_int o in
        if value > 0 then Ok (Constant value)
        else root_cause "Constant value must be greater than zero" j
    | "Dynamic" -> Ok Dynamic
    | _ -> root_cause "ArraySize" j
end

module Type = struct
  type t = { name : string option; inner : inner }

  and inner =
    | Scalar of Scalar.t
    | Vector of { size : VectorSize.t; scalar : Scalar.t }
    | Matrix of {
        columns : VectorSize.t;
        rows : VectorSize.t;
        scalar : Scalar.t;
      }
    | Atomic of Scalar.t
    | Pointer of { base : t; space : AddressSpace.t }
    | ValuePointer of {
        size : VectorSize.t option;
        scalar : Scalar.t;
        space : AddressSpace.t;
      }
    | Array of { base : t; size : ArraySize.t }
    | Struct of { members : struct_member list; span : int }
    | Image of {
        dim : ImageDimension.t;
        arrayed : bool;
        image_class : ImageClass.t;
      }
    | Sampler of { comparison : bool }
    | AccelerationStructure
    | BindingArray of { base : t; size : ArraySize.t }

  and struct_member = {
    name : string;
    ty : t;
    binding : Binding.t option;
    offset : int;
  }

  let i_vec3_u32 : inner = Vector { size = VectorSize.Tri; scalar = Scalar.u32 }
  let make (inner : inner) : t = { name = None; inner }

  let update_scalar (f : Scalar.t -> Scalar.t) (ty : t) : t =
    match ty.inner with
    | Scalar s -> make (Scalar (f s))
    | Vector { size; scalar } -> make (Vector { size; scalar = f scalar })
    | Matrix { columns; rows; scalar } ->
        make (Matrix { columns; rows; scalar = f scalar })
    | Atomic s -> make (Atomic (f s))
    | ValuePointer { size; scalar; space } ->
        make (ValuePointer { size; scalar = f scalar; space })
    | Pointer _ | Array _ | Struct _ | AccelerationStructure | BindingArray _
    | Image _ | Sampler _ ->
        ty

  let to_scalar (ty : t) : Scalar.t option =
    match ty.inner with Scalar s -> Some s | _ -> None

  let is_array (ty : t) : bool =
    match ty.inner with Array _ -> true | _ -> false

  let is_vector (ty : t) : bool =
    match ty.inner with Vector _ -> true | _ -> false

  let is_scalar (ty : t) : bool =
    match ty.inner with Scalar _ -> true | _ -> false

  let is_int (ty : t) : bool =
    ty |> to_scalar |> Option.map Scalar.is_int |> Option.value ~default:false

  let is_bool (ty : t) : bool =
    ty |> to_scalar |> Option.map Scalar.is_bool |> Option.value ~default:false

  let is_vec3_u32 (ty : t) : bool = ty.inner = i_vec3_u32
  let vector_field : string list = [ "x"; "y"; "z"; "w" ]

  let lookup_field (index : int) (ty : t) : string option =
    match ty.inner with
    | Vector _ -> List.nth_opt vector_field index
    | Struct { members; _ } ->
        index |> List.nth_opt members
        |> Option.map (fun (m : struct_member) -> m.name)
    | _ -> None

  let scalar (s : Scalar.t) : t = make (Scalar s)
  let i32 : t = scalar Scalar.i32
  let u32 : t = scalar Scalar.u32
  let u64 : t = scalar Scalar.u64
  let f32 : t = scalar Scalar.f32
  let f64 : t = scalar Scalar.f64

  let vec (size : int) (scalar : Scalar.t) : t =
    let size = VectorSize.from_int size |> Option.get in
    make (Vector { size; scalar })

  let vec4_u32 : t = vec 4 Scalar.u32
  let bool : t = scalar Scalar.bool

  let array ?(size = ArraySize.Dynamic) (base : t) : t =
    make (Array { base; size })

  let modf_result (ty : t) : t =
    make
      (Struct
         {
           members =
             [
               { name = "fract"; ty; binding = None; offset = 0 };
               { name = "whole"; ty; binding = None; offset = 0 };
             ];
           span = 0;
         })

  (** For container types, return the type of the contained elements. Does not
      support structs. *)
  let deref (ty : t) : t option =
    match ty.inner with
    | ValuePointer { scalar = s; _ } | Vector { scalar = s; _ } ->
        Some (scalar s)
    | Matrix { columns; scalar; _ } ->
        Some (make (Vector { size = columns; scalar }))
    | BindingArray { base; _ } | Array { base; _ } -> Some base
    | _ -> None

  (** Get the i-th type; if it's a struct look up the field type, otherwise
      deref. *)
  let nth (index : int) (ty : t) : t option =
    match ty.inner with
    | Struct { members; _ } -> Some (List.nth members index).ty
    | _ -> deref ty

  (* Deref n-dimensions, rather than just one dimension, whic his what
      deref does. *)
  let deref_list (index : int option list) (ty : t) : t option =
    let ( let* ) = Option.bind in
    (* we need to deref as many times as indices *)
    let rec iter (l : int option list) (ty : t) : t option =
      match l with
      | [] -> Some ty
      | o :: l ->
          let* ty = match o with Some n -> nth n ty | None -> deref ty in
          iter l ty
    in
    iter index ty

  let rec inner_to_string (name : string option) : inner -> string = function
    | Scalar s -> Scalar.to_string s
    | Array a ->
        let size =
          match a.size with
          | Constant i -> ", " ^ string_of_int i
          | Dynamic -> ""
        in
        "array<" ^ to_string a.base ^ size ^ ">"
    | Vector v ->
        "vec"
        ^ VectorSize.to_string v.size
        ^ "<" ^ Scalar.to_string v.scalar ^ ">"
    | Image { dim; arrayed; image_class } ->
        let dim = ImageDimension.to_string dim in
        let arrayed = if arrayed then "_array" else "" in
        let klass, format, storage =
          match image_class with
          | Sampled { kind; _ } ->
              let scalar = { kind; width = 4 } |> Scalar.to_string in
              ("", scalar, "")
          | Depth _ -> ("depth_", "", "")
          | Storage { format; access } ->
              ( "storage_",
                StorageFormat.to_string format,
                StorageAccess.to_string access )
        in
        let multi =
          if ImageClass.multisampled image_class then "multisampled_" else ""
        in
        let addendum =
          if format <> "" then "<" ^ format ^ storage ^ ">" else ""
        in
        "texture_" ^ klass ^ multi ^ dim ^ arrayed ^ addendum
    | Struct { members = m; _ } ->
        let name = match name with Some name -> name ^ " " | None -> "" in
        "struct " ^ name ^ "{"
        ^ (List.map struct_to_string m |> String.concat ", ")
        ^ "};"
    | Matrix { columns; rows; scalar } ->
        let columns = VectorSize.to_string columns in
        let rows = VectorSize.to_string rows in
        "mat" ^ rows ^ "x" ^ columns ^ "<" ^ Scalar.to_string scalar ^ ">"
    | Atomic s -> Printf.sprintf "atomic<%s>" (Scalar.to_string s)
    | Pointer { base = b; space = s } ->
        Printf.sprintf "ptr<%s,%s>" (to_string b) (AddressSpace.to_string s)
    | Sampler { comparison } ->
        if comparison then "sampler_comparison" else "sampler"
    | BindingArray { base; size } -> (
        let base = to_string base in
        match size with
        | Dynamic -> Printf.sprintf "binding_array<%s>" base
        | Constant k -> Printf.sprintf "binding_array<%s,%d>" base k)
    | ValuePointer { size; scalar; space } ->
        let args =
          (size
          |> Option.map (fun s -> s |> VectorSize.to_int |> string_of_int)
          |> Option.to_list)
          @ [ Scalar.to_string scalar; AddressSpace.to_string space ]
          |> String.concat ", "
        in
        Printf.sprintf "ptr<%s>" args
    | AccelerationStructure -> "acceleration_structure"

  and to_string (e : t) : string =
    match e.name with Some n -> n | None -> inner_to_string None e.inner

  and struct_to_string (s : struct_member) : string =
    let binding =
      match s.binding with Some b -> Binding.to_string b ^ " " | None -> ""
    in
    binding ^ s.name ^ " : " ^ to_string s.ty

  let frexp_result (ty : t) : t =
    let fract, exp =
      match ty.inner with
      | Scalar { kind = Float; width = (4 | 2) as w } ->
          (scalar { kind = Float; width = w }, i32)
      | Vector { scalar = { kind = Float; width = (4 | 2) as w }; size } ->
          ( make (Vector { scalar = { kind = Float; width = w }; size }),
            make (Vector { scalar = Scalar.i32; size }) )
      | Scalar { kind = AbstractFloat; width } ->
          ( scalar { kind = AbstractFloat; width },
            scalar { kind = AbstractInt; width } )
      | Vector { scalar = { kind = AbstractFloat; width }; size } ->
          ( make (Vector { scalar = { kind = AbstractFloat; width }; size }),
            make (Vector { scalar = { kind = AbstractInt; width }; size }) )
      | _ -> failwith ("frexp_result: " ^ to_string ty)
    in
    make
      (Struct
         {
           members =
             [
               { name = "fract"; ty = fract; binding = None; offset = 0 };
               { name = "exp"; ty = exp; binding = None; offset = 0 };
             ];
           span = 0;
         })

  let access_index (index : int) (base : t) : t =
    let fail () =
      failwith
        (Printf.sprintf "Type.access_index(index=%d, base=%s)" index
           (to_string base))
    in
    match base.inner with
    (* Handle indexing a vector *)
    | Vector { size = _; scalar } -> make (Scalar scalar)
    (* Handle indexing a matrix *)
    | Matrix { columns = _; rows; scalar } ->
        (* Guarantee that index is within columns *)
        make (Vector { size = rows; scalar })
    (* Handle indexing an array *)
    | Array { base; _ } -> base
    (* Handle indexing a struct *)
    | Struct { members; _ } ->
        let member =
          match List.nth_opt members index with Some m -> m | None -> fail ()
        in
        member.ty
    (* Handle indexing a value pointer with size *)
    | ValuePointer { size = Some _; scalar; space } ->
        make (ValuePointer { size = None; scalar; space })
    (* Handle indexing a pointer *)
    | Pointer { base; space } ->
        make
          (match base.inner with
          | Array { base; _ } -> Pointer { base; space }
          | Vector { size = _; scalar } ->
              ValuePointer { size = None; scalar; space }
          | Matrix { rows; columns = _; scalar } ->
              ValuePointer { size = Some rows; scalar; space }
          | Struct { members; _ } ->
              let member =
                match List.nth_opt members index with
                | Some m -> m
                | None -> fail ()
              in
              Pointer { base = member.ty; space }
          | BindingArray { base; _ } -> Pointer { base; space }
          | _ -> fail ())
    (* Handle indexing a binding array *)
    | BindingArray { base; _ } -> base
    (* Handle unexpected types *)
    | _ -> fail ()

  let access (base : t) : t =
    let fail () =
      failwith (Printf.sprintf "Type.access(%s)" (to_string base))
    in
    match base.inner with
    | Array { base; _ } | BindingArray { base; _ } -> base
    | Matrix { rows; scalar; _ } -> make (Vector { size = rows; scalar })
    | Vector { size = _; scalar } -> make (Scalar scalar)
    | ValuePointer { size = Some _; scalar; space } ->
        make (ValuePointer { size = None; scalar; space })
    (* Handle pointers that can point to different types *)
    | Pointer { base; space } ->
        make
          (match base.inner with
          | Array { base; _ } -> Pointer { base; space }
          | Vector { size = _; scalar } ->
              ValuePointer { size = None; scalar; space }
          | Matrix { columns = _; rows; scalar } ->
              ValuePointer { size = Some rows; scalar; space }
          | BindingArray { base; _ } -> Pointer { base; space }
          | _ -> fail ())
    (* Handle unexpected types *)
    | _ -> fail ()

  let cast (kind : ScalarKind.t) (width : int option) (ty : t) : t =
    match ty.inner with
    | Scalar _ | Vector _ | Matrix _ ->
        (*
        Update the kind of the scalar.
        Additionally, if convert is set, then we update the scalar width.
      *)
        let update : Scalar.t -> Scalar.t =
          match width with
          | Some width -> fun _ -> { width; kind }
          | None -> fun s -> { s with kind }
        in
        update_scalar update ty
    | _ ->
        failwith
          (Printf.sprintf "Type.cast(kind=%s, width=%s)"
             (ScalarKind.to_string kind)
             (width |> Option.map string_of_int |> Option.value ~default:"null"))

  let swizzle (size : VectorSize.t) (ty : t) : t =
    match ty.inner with
    | Vector { scalar; _ } -> make (Vector { size; scalar })
    | _ ->
        failwith
          (Printf.sprintf "swizzle(size=%s, ty=%s)"
             (VectorSize.to_string size)
             (to_string ty))

  let splat (size : VectorSize.t) : t -> t = function
    | { inner = Scalar scalar; _ } -> make (Vector { size; scalar })
    | ty ->
        failwith
          (Printf.sprintf "splat(size=%s, value=%s)"
             (VectorSize.to_string size)
             (to_string ty))

  (** Return the type of the image operation associated with an image type. *)
  let image_class (ty : t) : t =
    match ty.inner with
    | Image { image_class; _ } -> (
        match image_class with
        | Depth _ -> f32
        | Sampled { kind; _ } -> vec 4 (Scalar.make_32 kind)
        | Storage { format; _ } ->
            let k = StorageFormat.to_kind format in
            vec 4 (Scalar.make_32 k))
    | _ -> failwith (Printf.sprintf "Type.image_class(%s)" (to_string ty))

  (** In the context of image queries of Size, return the type of the operation.
  *)
  let image_dim (ty : t) : t =
    match ty.inner with
    | Image { dim; _ } -> (
        match dim with
        | D1 -> u32
        | D2 | Cube -> vec 2 Scalar.u32
        | D3 -> vec 3 Scalar.u32)
    | _ -> failwith (Printf.sprintf "Type.image_dim(%s)" (to_string ty))

  let rec parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" (cast_option cast_string) o in
    let* inner = with_field "inner" inner_parse o in
    Ok { name; inner }

  and inner_parse (j : json) : inner j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Array" ->
        let* base = with_field "base" parse o in
        let* size = with_field "size" ArraySize.parse o in
        Ok (Array { base; size })
    | "Scalar" ->
        let* s = with_field "value" Scalar.parse o in
        Ok (Scalar s)
    | "Vector" ->
        let* size = with_field "size" VectorSize.parse o in
        let* scalar = with_field "scalar" Scalar.parse o in
        Ok (Vector { size; scalar })
    | "Image" ->
        let* dim = with_field "dim" ImageDimension.parse o in
        let* arrayed = with_field "arrayed" cast_bool o in
        let* image_class = with_field "class" ImageClass.parse o in
        Ok (Image { dim; arrayed; image_class })
    | "Struct" ->
        let* span = with_field "span" cast_int o in
        let* members = with_field "members" (cast_map struct_parse) o in
        Ok (Struct { span; members })
    | "Matrix" ->
        let* rows = with_field "rows" VectorSize.parse o in
        let* columns = with_field "columns" VectorSize.parse o in
        let* scalar = with_field "scalar" Scalar.parse o in
        Ok (Matrix { rows; columns; scalar })
    | "Atomic" ->
        let* scalar = with_field "scalar" Scalar.parse o in
        Ok (Atomic scalar)
    | "Pointer" ->
        let* base = with_field "base" parse o in
        let* space = with_field "space" AddressSpace.parse o in
        Ok (Pointer { base; space })
    | "Sampler" ->
        let* comparison = with_field "comparison" cast_bool o in
        Ok (Sampler { comparison })
    | "BindingArray" ->
        let* base = with_field "base" parse o in
        let* size = with_field "size" ArraySize.parse o in
        Ok (BindingArray { base; size })
    | _ -> root_cause ("inner_parse: unsupported kind: " ^ kind) j

  and struct_parse (j : json) : struct_member j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" parse o in
    let* binding = with_field "binding" (cast_option Binding.parse) o in
    let* offset = with_field "offset" cast_int o in
    Ok { name; ty; binding; offset }
end

let parse_var (o : Rjson.j_object) : Variable.t j_result =
  let open Rjson in
  let* name = with_field "name" cast_string o in
  Ok (Variable.from_name name)

module IdentKind = struct
  type t =
    | FunctionArgument of Binding.t option
    | GlobalVariable
    | LocalVariable
    | CallResult
    | Constant
    | Override
    | AtomicResult of { comparison : bool }
    | WorkGroupUniformLoadResult
    | SubgroupBallotResult
    | SubgroupOperationResult

  let local_invocation_id : t =
    FunctionArgument (Some Binding.local_invocation_id)

  let is_local_invocation_id (k : t) : bool = k = local_invocation_id
  let workgroup_id : t = FunctionArgument (Some Binding.workgroup_id)
  let is_workgroup_id (k : t) : bool = k = workgroup_id
  let num_workgroups : t = FunctionArgument (Some Binding.num_workgroups)
  let is_num_workgroups (k : t) : bool = k = num_workgroups

  let global_invocation_id : t =
    FunctionArgument (Some Binding.global_invocation_id)

  let is_global_invocation_id (k : t) = k = global_invocation_id

  let is_concurrency_related : t -> bool = function
    | FunctionArgument (Some b) -> Binding.is_concurrency_related b
    | _ -> false

  let call_result : Variable.t = Variable.from_name "@Call"
  let atomic_result : Variable.t = Variable.from_name "@Atomic"

  let work_group_uniform_load_result : Variable.t =
    Variable.from_name "@WorkGroupUniformLoad"

  let subgroup_ballot_result : Variable.t = Variable.from_name "@SubgroupBallot"

  let subgroup_operation_result : Variable.t =
    Variable.from_name "@SubgroupOperation"

  let default_var : t -> Variable.t option = function
    | CallResult -> Some call_result
    | AtomicResult _ -> Some atomic_result
    | WorkGroupUniformLoadResult -> Some work_group_uniform_load_result
    | SubgroupBallotResult -> Some subgroup_ballot_result
    | SubgroupOperationResult -> Some subgroup_operation_result
    | _ -> None

  let to_string : t -> string = function
    | FunctionArgument (Some x) -> "func " ^ Binding.to_string x
    | FunctionArgument None -> "func"
    | GlobalVariable -> "global"
    | LocalVariable -> "local"
    | CallResult -> "call"
    | Constant -> "const"
    | Override -> "override"
    | AtomicResult { comparison } ->
        "atomic" ^ if comparison then " comp" else ""
    | WorkGroupUniformLoadResult -> "work_group_uniform_load"
    | SubgroupBallotResult -> "subgroup_ballot"
    | SubgroupOperationResult -> "subgroup_operation"

  let is_kind : string -> bool = function
    | "FunctionArgument" | "GlobalVariable" | "LocalVariable" | "CallResult"
    | "Constant" | "Override" | "AtomicResult" | "WorkGroupUniformLoadResult"
    | "SubgroupBallotResult" | "SubgroupOperationResult" ->
        true
    | _ -> false

  let parse_kind (o : Rjson.j_object) : t j_result =
    let open Rjson in
    let* kind = get_kind o in
    match kind with
    | "FunctionArgument" ->
        let* binding = with_field "binding" (cast_option Binding.parse) o in
        Ok (FunctionArgument binding)
    | "GlobalVariable" -> Ok GlobalVariable
    | "LocalVariable" -> Ok LocalVariable
    | "CallResult" -> Ok CallResult
    | "Constant" -> Ok Constant
    | "Override" -> Ok Override
    | "WorkGroupUniformLoadResult" -> Ok WorkGroupUniformLoadResult
    | "AtomicResult" ->
        let* comparison = with_field "comparison" cast_bool o in
        Ok (AtomicResult { comparison })
    | "SubgroupBallotResult" -> Ok SubgroupBallotResult
    | "SubgroupOperationResult" -> Ok SubgroupOperationResult
    | _ -> root_cause ("Indent.parse: unknown kind: " ^ kind) (`Assoc o)

  let parse (o : Rjson.j_object) : (Variable.t * Type.t * t) j_result =
    let open Rjson in
    let* kind = parse_kind o in
    let* ty =
      if kind = SubgroupBallotResult then Ok Type.vec4_u32
      else with_field "ty" Type.parse o
    in
    let* var =
      (* Naga will set name to null when the result is the result
          of a previous call/atomic performed. We use @Call to represent
          the contents of the last write. *)
      match default_var kind with
      | Some v -> Ok v
      | None -> parse_var o
    in
    Ok (var, ty, kind)
end

let parse_location (j : json) : Location.t j_result =
  let open Rjson in
  let* o = cast_object j in
  let* filename = with_field "filename" cast_string o in
  let* line_number = with_field "line_number" cast_int o in
  let* line_position = with_field "line_position" cast_int o in
  let* length = with_field "length" cast_int o in
  Ok
    {
      Location.filename;
      line = Index.from_base1 line_number;
      interval =
        Interval.from_range ~start:(Index.from_base1 line_position) ~length;
    }

module Ident = struct
  type t = { var : Variable.t; ty : Type.t; kind : IdentKind.t }

  let is_function_argument (x : t) : bool =
    match x.kind with FunctionArgument _ -> true | _ -> false

  let is_concurrency_related (x : t) : bool =
    IdentKind.is_concurrency_related x.kind

  let is_global (x : t) : bool = x.kind = GlobalVariable
  let var (i : t) : Variable.t = i.var

  let set_location (l : Location.t) (x : t) : t =
    { x with var = Variable.set_location l x.var }

  let add_suffix (suffix : string) (x : t) =
    { x with var = Variable.add_suffix suffix x.var }

  let atomic_result ?(comparison = false) (ty : Type.t) : t =
    { var = IdentKind.call_result; ty; kind = AtomicResult { comparison } }

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* var, ty, kind = IdentKind.parse o in
    Ok { ty; var; kind }

  let to_string (x : t) : string =
    Printf.sprintf "%s: %s" (Variable.label x.var) (Type.to_string x.ty)

  let name (x : t) : string = x.var |> Variable.name
end

module FunctionArgument = struct
  type t = { name : string; ty : Type.t; binding : Binding.t option }

  let ty (f : t) : Type.t = f.ty
  let name (f : t) : string = f.name

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" Type.parse o in
    let* binding = with_field "binding" (cast_option Binding.parse) o in
    Ok { ty; binding; name }

  let to_string (e : t) : string =
    let binding =
      e.binding
      |> Option.map (fun b -> "@" ^ Binding.to_string b ^ " ")
      |> Option.value ~default:""
    in
    binding ^ e.name ^ ": " ^ Type.to_string e.ty
end

module FunctionResult = struct
  type t = { ty : Type.t; binding : Binding.t option }

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* ty = with_field "ty" Type.parse o in
    let* binding = with_field "binding" (cast_option Binding.parse) o in
    Ok { ty; binding }
end

module UnaryOperator = struct
  type t = Negate | LogicalNot | BitwiseNot

  let to_string : t -> string = function
    | Negate -> "-"
    | LogicalNot -> "!"
    | BitwiseNot -> "~"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Negate" -> Ok Negate
    | "LogicalNot" -> Ok LogicalNot
    | "BitwiseNot" -> Ok BitwiseNot
    | _ -> root_cause "UnaryOperator" j
end

module BinaryOperator = struct
  type t =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equal
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | And
    | ExclusiveOr
    | InclusiveOr
    | LogicalAnd
    | LogicalOr
    | ShiftLeft
    | ShiftRight

  let to_string : t -> string = function
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Modulo -> "%"
    | Equal -> "=="
    | NotEqual -> "!="
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | And -> "&"
    | ExclusiveOr -> "^"
    | InclusiveOr -> "|"
    | LogicalAnd -> "&&"
    | LogicalOr -> "||"
    | ShiftLeft -> "<<"
    | ShiftRight -> ">>"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Add" -> Ok Add
    | "Subtract" -> Ok Subtract
    | "Multiply" -> Ok Multiply
    | "Divide" -> Ok Divide
    | "Modulo" -> Ok Modulo
    | "Equal" -> Ok Equal
    | "NotEqual" -> Ok NotEqual
    | "Less" -> Ok Less
    | "LessEqual" -> Ok LessEqual
    | "Greater" -> Ok Greater
    | "GreaterEqual" -> Ok GreaterEqual
    | "And" -> Ok And
    | "ExclusiveOr" -> Ok ExclusiveOr
    | "InclusiveOr" -> Ok InclusiveOr
    | "LogicalAnd" -> Ok LogicalAnd
    | "LogicalOr" -> Ok LogicalOr
    | "ShiftLeft" -> Ok ShiftLeft
    | "ShiftRight" -> Ok ShiftRight
    | _ -> root_cause "BinaryOperator" j

  (** Given the type of both operations return the return type. *)
  let type_of (op : t) (lhs : Type.t) (rhs : Type.t) : Type.t =
    match (op, lhs.inner, rhs.inner) with
    (* Handle basic arithmetic operators that return the type of the left operand *)
    | ( ( Add | Subtract | Divide | Modulo | And | ExclusiveOr | InclusiveOr
        | ShiftLeft | ShiftRight ),
        ty,
        _ ) ->
        Type.make ty
    (* Handle multiplication, which requires more detailed type resolution,
       due to matrix-matrix and matrix-vector, scalar-vector/matrix/scalar *)
    | Multiply, Matrix { columns = _; rows; scalar }, Matrix { columns; _ } ->
        Type.make (Matrix { columns; rows; scalar })
    | Multiply, Matrix { columns = _; rows; scalar }, Vector _ ->
        Type.make (Vector { size = rows; scalar })
    | Multiply, Vector _, Matrix { columns; rows = _; scalar } ->
        Type.make (Vector { size = columns; scalar })
    | Multiply, Scalar _, ty
    | Multiply, ty, Scalar _
    | Multiply, (Vector _ as ty), Vector _ ->
        Type.make ty
    (* Handle comparison and logical operators, which return boolean values *)
    | ( ( Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
        | LogicalAnd | LogicalOr ),
        ((Scalar _ | Vector _) as ty),
        _ ) ->
        let scalar = Scalar.bool in
        let inner : Type.inner =
          match ty with
          | Scalar _ -> Scalar scalar
          | Vector { size; _ } -> Vector { size; scalar }
          | _ -> failwith "impossible"
        in
        Type.make inner
    | _, _, _ -> failwith "BinaryOperator.type_of"
end

module Literal = struct
  type t =
    | F32 of float
    | F64 of float
    | U32 of int
    | U64 of int
    | I32 of int
    | I64 of int
    | AbstractInt of int
    | AbstractFloat of float
    | Bool of bool

  let to_bool : t -> bool option = function
    | F64 _ | F32 _ | U32 _ | I32 _ | U64 _ | I64 _ | AbstractInt _
    | AbstractFloat _ ->
        None
    | Bool b -> Some b

  let to_int : t -> int option = function
    | U32 v | I32 v | U64 v | I64 v | AbstractInt v -> Some v
    | F64 _ | F32 _ | AbstractFloat _ | Bool _ -> None

  let to_float : t -> float option = function
    | Bool _ | U32 _ | I32 _ | U64 _ | I64 _ | AbstractInt _ -> None
    | F64 v | F32 v | AbstractFloat v -> Some v

  let to_string : t -> string = function
    | F32 v -> Float.to_string v
    | F64 v -> Float.to_string v
    | U32 v -> Int.to_string v
    | I32 v -> Int.to_string v
    | U64 v -> Int.to_string v
    | I64 v -> Int.to_string v
    | Bool v -> Bool.to_string v
    | AbstractInt v -> Int.to_string v
    | AbstractFloat v -> Float.to_string v

  let scalar_of : t -> Scalar.t = function
    | F32 _ -> Scalar.f32
    | F64 _ -> Scalar.f64
    | U32 _ -> Scalar.u32
    | I32 _ -> Scalar.i32
    | U64 _ -> Scalar.u64
    | I64 _ -> Scalar.i64
    | Bool _ -> Scalar.bool
    | AbstractInt _ -> Scalar.int
    | AbstractFloat _ -> Scalar.float

  let type_of (l : t) : Type.t = Type.scalar (scalar_of l)

  let zero (s : Scalar.t) : t =
    match s.kind with
    | Sint -> if s.width <= 4 then I32 0 else I64 0
    | Uint -> if s.width <= 4 then U32 0 else U64 0
    | AbstractInt -> AbstractInt 0
    | Float -> if s.width <= 4 then F32 0.0 else F64 0.0
    | AbstractFloat -> AbstractFloat 0.0
    | Bool -> Bool false

  let bool (b : bool) : t = Bool b
  let true_ : t = Bool true
  let false_ : t = Bool false
  let i32 (i : int) : t = I32 i
  let int (i : int) : t = AbstractInt i
  let float (f : float) : t = AbstractFloat f

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "f64" ->
        let* v = with_field "value" cast_float o in
        Ok (F64 v)
    | "f32" ->
        let* v = with_field "value" cast_float o in
        Ok (F32 v)
    | "u32" ->
        let* v = with_field "value" cast_int o in
        Ok (U32 v)
    | "i32" ->
        let* v = with_field "value" cast_int o in
        Ok (I32 v)
    | "u64" ->
        let* v = with_field "value" cast_int o in
        Ok (U64 v)
    | "i64" ->
        let* v = with_field "value" cast_int o in
        Ok (I64 v)
    | "bool" ->
        let* v = with_field "value" cast_bool o in
        Ok (Bool v)
    | "int" ->
        let* v = with_field "value" cast_int o in
        Ok (AbstractInt v)
    | "float" ->
        let* v = with_field "value" cast_float o in
        Ok (AbstractFloat v)
    | _ -> failwith kind
end

module MathFunction = struct
  type t =
    | Abs
    | Min
    | Max
    | Clamp
    | Saturate
    | Cos
    | Cosh
    | Sin
    | Sinh
    | Tan
    | Tanh
    | Acos
    | Asin
    | Atan
    | Atan2
    | Asinh
    | Acosh
    | Atanh
    | Radians
    | Degrees
    | Ceil
    | Floor
    | Round
    | Fract
    | Trunc
    | Modf
    | Frexp
    | Ldexp
    | Exp
    | Exp2
    | Log
    | Log2
    | Pow
    | Dot
    | Cross
    | Distance
    | Length
    | Normalize
    | FaceForward
    | Reflect
    | Refract
    | Sign
    | Fma
    | Mix
    | Step
    | SmoothStep
    | Sqrt
    | InverseSqrt
    | Transpose
    | Determinant
    | CountTrailingZeros
    | CountLeadingZeros
    | CountOneBits
    | ReverseBits
    | ExtractBits
    | InsertBits
    | FindLsb
    | FindMsb
    | Pack4x8snorm
    | Pack4x8unorm
    | Pack2x16snorm
    | Pack2x16unorm
    | Pack2x16float
    | Pack4xI8
    | Pack4xU8
    | Unpack4x8snorm
    | Unpack4x8unorm
    | Unpack2x16snorm
    | Unpack2x16unorm
    | Unpack2x16float
    | Unpack4xI8
    | Unpack4xU8

  let parse (j : json) : t j_result =
    let open Rjson in
    let* kind = cast_string j in
    match kind with
    | "Abs" -> Ok Abs
    | "Min" -> Ok Min
    | "Max" -> Ok Max
    | "Clamp" -> Ok Clamp
    | "Saturate" -> Ok Saturate
    | "Cos" -> Ok Cos
    | "Cosh" -> Ok Cosh
    | "Sin" -> Ok Sin
    | "Sinh" -> Ok Sinh
    | "Tan" -> Ok Tan
    | "Tanh" -> Ok Tanh
    | "Acos" -> Ok Acos
    | "Asin" -> Ok Asin
    | "Atan" -> Ok Atan
    | "Atan2" -> Ok Atan2
    | "Asinh" -> Ok Asinh
    | "Acosh" -> Ok Acosh
    | "Atanh" -> Ok Atanh
    | "Radians" -> Ok Radians
    | "Degrees" -> Ok Degrees
    | "Ceil" -> Ok Ceil
    | "Floor" -> Ok Floor
    | "Round" -> Ok Round
    | "Fract" -> Ok Fract
    | "Trunc" -> Ok Trunc
    | "Modf" -> Ok Modf
    | "Frexp" -> Ok Frexp
    | "Ldexp" -> Ok Ldexp
    | "Exp" -> Ok Exp
    | "Exp2" -> Ok Exp2
    | "Log" -> Ok Log
    | "Log2" -> Ok Log2
    | "Pow" -> Ok Pow
    | "Dot" -> Ok Dot
    | "Cross" -> Ok Cross
    | "Distance" -> Ok Distance
    | "Length" -> Ok Length
    | "Normalize" -> Ok Normalize
    | "FaceForward" -> Ok FaceForward
    | "Reflect" -> Ok Reflect
    | "Refract" -> Ok Refract
    | "Sign" -> Ok Sign
    | "Fma" -> Ok Fma
    | "Mix" -> Ok Mix
    | "Step" -> Ok Step
    | "SmoothStep" -> Ok SmoothStep
    | "Sqrt" -> Ok Sqrt
    | "InverseSqrt" -> Ok InverseSqrt
    | "Transpose" -> Ok Transpose
    | "Determinant" -> Ok Determinant
    | "CountTrailingZeros" -> Ok CountTrailingZeros
    | "CountLeadingZeros" -> Ok CountLeadingZeros
    | "CountOneBits" -> Ok CountOneBits
    | "ReverseBits" -> Ok ReverseBits
    | "ExtractBits" -> Ok ExtractBits
    | "InsertBits" -> Ok InsertBits
    | "FindLsb" -> Ok FindLsb
    | "FindMsb" -> Ok FindMsb
    | "Pack4x8snorm" -> Ok Pack4x8snorm
    | "Pack4x8unorm" -> Ok Pack4x8unorm
    | "Pack2x16snorm" -> Ok Pack2x16snorm
    | "Pack2x16unorm" -> Ok Pack2x16unorm
    | "Pack2x16float" -> Ok Pack2x16float
    | "Pack4xI8" -> Ok Pack4xI8
    | "Pack4xU8" -> Ok Pack4xU8
    | "Unpack4x8snorm" -> Ok Unpack4x8snorm
    | "Unpack4x8unorm" -> Ok Unpack4x8unorm
    | "Unpack2x16snorm" -> Ok Unpack2x16snorm
    | "Unpack2x16unorm" -> Ok Unpack2x16unorm
    | "Unpack2x16float" -> Ok Unpack2x16float
    | "Unpack4xI8" -> Ok Unpack4xI8
    | "Unpack4xU8" -> Ok Unpack4xU8
    | _ -> root_cause "MathFunction" j

  let to_string : t -> string = function
    | Abs -> "abs"
    | Min -> "min"
    | Max -> "max"
    | Clamp -> "clamp"
    | Saturate -> "saturate"
    | Cos -> "cos"
    | Cosh -> "cosh"
    | Sin -> "sin"
    | Sinh -> "sinh"
    | Tan -> "tan"
    | Tanh -> "tanh"
    | Acos -> "acos"
    | Asin -> "asin"
    | Atan -> "atan"
    | Atan2 -> "atan2"
    | Asinh -> "asinh"
    | Acosh -> "acosh"
    | Atanh -> "atanh"
    | Radians -> "radians"
    | Degrees -> "degrees"
    | Ceil -> "ceil"
    | Floor -> "floor"
    | Round -> "round"
    | Fract -> "fract"
    | Trunc -> "trunc"
    | Modf -> "modf"
    | Frexp -> "frexp"
    | Ldexp -> "ldexp"
    | Exp -> "exp"
    | Exp2 -> "exp2"
    | Log -> "log"
    | Log2 -> "log2"
    | Pow -> "pow"
    | Dot -> "dot"
    | Cross -> "cross"
    | Distance -> "distance"
    | Length -> "length"
    | Normalize -> "normalize"
    | FaceForward -> "faceForward"
    | Reflect -> "reflect"
    | Refract -> "refract"
    | Sign -> "sign"
    | Fma -> "fma"
    | Mix -> "mix"
    | Step -> "step"
    | SmoothStep -> "smoothstep"
    | Sqrt -> "sqrt"
    | InverseSqrt -> "inverseSqrt"
    | Transpose -> "transpose"
    | Determinant -> "determinant"
    | CountTrailingZeros -> "countTrailingZeros"
    | CountLeadingZeros -> "countLeadingZeros"
    | CountOneBits -> "countOneBits"
    | ReverseBits -> "reverseBits"
    | ExtractBits -> "extractBits"
    | InsertBits -> "insertBits"
    | FindLsb -> "firstTrailingBit"
    | FindMsb -> "firstLeadingBit"
    | Pack4x8snorm -> "pack4x8snorm"
    | Pack4x8unorm -> "pack4x8unorm"
    | Pack2x16snorm -> "pack2x16snorm"
    | Pack2x16unorm -> "pack2x16unorm"
    | Pack2x16float -> "pack2x16float"
    | Pack4xI8 -> "pack4xI8"
    | Pack4xU8 -> "pack4xU8"
    | Unpack4x8snorm -> "unpack4x8snorm"
    | Unpack4x8unorm -> "unpack4x8unorm"
    | Unpack2x16snorm -> "unpack2x16snorm"
    | Unpack2x16unorm -> "unpack2x16unorm"
    | Unpack2x16float -> "unpack2x16float"
    | Unpack4xI8 -> "unpack4xI8"
    | Unpack4xU8 -> "unpack4xU8"

  (*
    Logic taken from:
    https://github.com/gfx-rs/wgpu/blob/f669024eeb9e86e553eb955a02b18a7723fb64e1/naga/src/proc/typifier.rs#L640
    *)
  let type_of : t -> Type.t list -> Type.t =
   fun op args ->
    match (op, args) with
    (* Modf returns a modf_result *)
    | Modf, [ arg0 ] -> Type.modf_result arg0
    (* Frexp returns a frexp_result *)
    | Frexp, [ arg0 ] -> Type.frexp_result arg0
    (* Handle the Dot function, which returns a scalar based on vector input *)
    | Dot, [ { inner = Vector { scalar; _ }; _ }; _ ] ->
        Type.make (Scalar scalar)
    (* Handle Distance and Length functions, which return a scalar based on the input *)
    | ( Distance,
        [ { inner = Scalar scalar | Vector { scalar; size = _ }; _ }; _ ] ) ->
        Type.make (Scalar scalar)
    (* Handle Distance and Length functions, which return a scalar based on the input *)
    | Length, [ { inner = Scalar scalar | Vector { scalar; size = _ }; _ } ] ->
        Type.make (Scalar scalar)
    (* Handle functions that normalize or manipulate vector inputs, returning the same type *)
    | ( ( Abs | Min | Max | Clamp | Saturate | Cos | Cosh | Sin | Sinh | Tan
        | Tanh | Acos | Asin | Atan | Atan2 | Asinh | Acosh | Atanh | Radians
        | Degrees | Ceil | Floor | Round | Fract | Trunc | Ldexp | Exp | Exp2
        | Log | Log2 | Pow | Cross | Normalize | FaceForward | Reflect | Refract
        | Sign | Fma | Mix | Step | SmoothStep | Sqrt | InverseSqrt ),
        arg0 :: _ ) ->
        arg0
    (* Handle Transpose, which swaps the rows and columns of a matrix *)
    | Transpose, [ { inner = Matrix { columns; rows; scalar }; _ } ] ->
        Type.make (Matrix { columns = rows; rows = columns; scalar })
    (* Handle Determinant, which returns a scalar based on a matrix input *)
    | Determinant, [ { inner = Matrix { scalar; _ }; _ } ] ->
        Type.make (Scalar scalar)
    (* Handle bit manipulation functions, which can operate on scalars or vectors *)
    | ( ( CountTrailingZeros | CountLeadingZeros | CountOneBits | ReverseBits
        | ExtractBits | InsertBits ),
        [ { inner = (Scalar s | Vector { scalar = s; _ }) as a0; _ } ] )
      when Scalar.is_int s ->
        Type.make a0
    (* Handle data packing functions, which return a scalar *)
    | ( ( Pack4x8snorm | Pack4x8unorm | Pack2x16snorm | Pack2x16unorm
        | Pack2x16float | Pack4xI8 | Pack4xU8 ),
        [ _ ] ) ->
        Type.u32
    (* Handle data unpacking functions, which return a vector *)
    | (Unpack4x8snorm | Unpack4x8unorm), [ _ ] -> Type.(vec 4 Scalar.f32)
    | (Unpack2x16snorm | Unpack2x16unorm | Unpack2x16float), [ _ ] ->
        Type.(vec 2 Scalar.f32)
    | Unpack4xI8, [ _ ] -> Type.(vec 4 Scalar.i32)
    | Unpack4xU8, [ _ ] -> Type.(vec 4 Scalar.u32)
    | _, _ -> failwith "MathFunction.type_of"
end

module RelationalFunction = struct
  type t = All | Any

  let to_string : t -> string = function All -> "all" | Any -> "any"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* kind = cast_string j in
    match kind with
    | "All" -> Ok All
    | "Any" -> Ok Any
    | _ -> root_cause "RelationalFunction" j
end

(** {1 DerivativeAxis Module}

    This module defines a type representing axes for derivatives, which can be
    used to specify which axis to compute a derivative along, such as {b x},
    {b y}, or {b width}. This concept is useful in WGSL where derivatives are
    often computed for operations like texture filtering or gradient evaluation.

    Example: A WGSL shader may compute partial derivatives using `dpdx` or
    `dpdy` to determine rate-of-change along the X or Y axis. *)

module DerivativeAxis = struct
  (** {2 Type: [t]}
      This type represents the possible derivative axes. *)
  type t =
    | X  (** Derivative along the X-axis. *)
    | Y  (** Derivative along the Y-axis. *)
    | Width  (** Derivative considering a range or width. *)

  (** {2 Function: [to_string]}

      [to_string axis] converts a [DerivativeAxis.t] value to its string
      representation. This can be useful when serializing data or logging the
      selected axis.

      @param axis The axis to convert to string.
      @return A string representing the axis ("x", "y", or "width").

      Example:
      {[
        let axis_str = DerivativeAxis.to_string X (* "x" *)
      ]} *)
  let to_string : t -> string = function
    | X -> "dpdx"
    | Y -> "dpdy"
    | Width -> "fwidth"

  (** {2 Function: [parse]}

      [parse j] attempts to parse a JSON value into a [DerivativeAxis.t]. If the
      JSON string matches "X", "Y", or "Width", the corresponding variant is
      returned. Otherwise, it returns an error.

      This function is useful when interpreting configuration or input data for
      shader derivatives.

      @param j A JSON value containing the axis name as a string.
      @return
        A result containing either a parsed [DerivativeAxis.t] or an error
        message.

      Example:
      {[
        let json_value = `String "X" in
        match DerivativeAxis.parse json_value with
        | Ok X -> print_endline "Parsed X-axis"
        | Error msg -> print_endline ("Error: " ^ msg)
      ]} *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "X" -> Ok X
    | "Y" -> Ok Y
    | "Width" -> Ok Width
    | _ -> root_cause "DerivativeAxis" j
end

(** Module for representing and parsing derivative control options used in WGSL.

    In WGSL, derivative control affects how partial derivatives are computed,
    with options such as coarse, fine, or none. These options are useful in
    controlling how shaders handle level-of-detail (LOD) computations and other
    operations that rely on derivatives.

    @see <https://www.w3.org/TR/WGSL/#derivatives>
      WGSL Derivative Control Specification *)
module DerivativeControl = struct
  (** Type representing derivative control options.
      - [Coarse]: Computes derivatives at a coarse granularity, potentially
        faster.
      - [Fine]: Computes derivatives at a finer granularity for better
        precision.
      - [None]: No derivative control applied.

      These are analogous to settings in WGSL for controlling the granularity of
      partial derivative calculations. *)
  type t =
    | Coarse  (** Coarse derivative control. *)
    | Fine  (** Fine-grained derivative control. *)
    | None  (** No derivative control. *)

  (**
    Converts a derivative control option to its string representation.
    Useful for logging or serializing the control option.

    @param t The derivative control option.
    @return A string corresponding to the derivative control option.

    @example
    {[
      let d = DerivativeControl.Fine in
      let str = DerivativeControl.to_string d in
      (* str will be "fine" *)
    ]}
  *)
  let to_string : t -> string = function
    | Coarse -> "Coarse"
    | Fine -> "Fine"
    | None -> ""

  (**
    Parses a JSON value into a derivative control option.
    This function expects the JSON input to be a string matching one of
    the control options ["Coarse"], ["Fine"], or ["None"].

    @param j A JSON value.
    @return A result containing the parsed derivative control option or an error.

    @example
    {[
      let json = `String "Coarse" in
      match DerivativeControl.parse json with
      | Ok control -> (* control is Coarse *)
      | Error e -> (* handle parsing error *)
    ]}
  *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Coarse" -> Ok Coarse
    | "Fine" -> Ok Fine
    | "None" -> Ok None
    | _ -> root_cause "DerivativeControl" j
end

let repeat (x : 'a) (count : int) : 'a list = List.init count (fun _ -> x)

module Expression = struct
  type t =
    | Literal of Literal.t
    | ZeroValue of Type.t
    | Compose of { ty : Type.t; components : t list }
    | Access of { base : t; index : t; location : Location.t }
    | AccessIndex of { base : t; index : int; location : Location.t }
    | Splat of { size : VectorSize.t; value : t }
    | Swizzle of {
        size : VectorSize.t;
        vector : t;
        indices : int list;
        location : Location.t;
      }
    | Ident of Ident.t
    | Load of t
    | ImageSample of {
        image : t;
        sampler : t;
        gather : int option;
        coordinate : t;
        array_index : t option;
        offset : t option;
        level : sample_level;
        depth_ref : t option;
      }
    | ImageLoad of {
        image : t;
        coordinate : t;
        array_index : t option;
        sample : t option;
        level : t option;
      }
    | ImageQuery of { image : t; query : image_query }
    | Unary of { op : UnaryOperator.t; expr : t }
    | Binary of { op : BinaryOperator.t; left : t; right : t }
    | Select of { condition : t; accept : t; reject : t }
    | Derivative of {
        axis : DerivativeAxis.t;
        ctrl : DerivativeControl.t;
        expr : t;
      }
    | Relational of { fun_ : RelationalFunction.t; argument : t }
    | Math of { fun_ : MathFunction.t; args : t list }
    | As of { expr : t; kind : ScalarKind.t; convert : int option }
    | ArrayLength of t

  and image_query = Size of t option | NumLevels | NumLayers | NumSamples

  (** The type [sample_level] represents the different ways of controlling
      texture sampling level of detail in WGSL.

      In WGSL, sampling operations can be modified with various sample level
      parameters, such as automatic selection, explicit bias, or gradients. This
      module provides types for these options and functions to convert them to
      string representations and parse them from JSON.

      Example WGSL usage:
      {ul
       {- In WGSL, when sampling from a texture with `textureSampleLevel`, you
          can specify levels like auto or explicit bias:
          {[
            textureSampleLevel (sampler, coords, level)
          ]}
       }
      } *)
  and sample_level =
    | Auto
        (** Use the automatic level of detail (LOD). This is the default in
            WGSL. *)
    | Zero  (** Use a fixed LOD of zero. *)
    | Exact of t
        (** Use an exact, explicit LOD. This corresponds to providing a specific
            LOD value. *)
    | Bias of t
        (** Apply a bias to the LOD. This is useful in mipmapping to offset the
            LOD. *)
    | Gradient of {
        x : t;  (** The x-component of the gradient. *)
        y : t;  (** The y-component of the gradient. *)
      }

  let i32 (i : int) : t = Literal (Literal.i32 i)
  let int (i : int) : t = Literal (Literal.int i)
  let float (f : float) : t = Literal (Literal.float f)

  (*
    Implementation follows Naga's type checker:

    https://github.com/gfx-rs/wgpu/blob/f669024eeb9e86e553eb955a02b18a7723fb64e1/naga/src/proc/typifier.rs#L238
  *)

  (** Returns the type of the given expression. *)
  let rec type_of : t -> Type.t = function
    | AccessIndex { base; index; _ } -> Type.access_index index (type_of base)
    | Access { base; _ } -> Type.access (type_of base)
    | Literal l -> Literal.type_of l
    | ZeroValue ty | Compose { ty; _ } -> ty
    | Ident i -> i.ty
    | Load e -> type_of e
    | Binary { op; left; right } ->
        BinaryOperator.type_of op (type_of left) (type_of right)
    | Select { accept = e; _ } -> type_of e
    | As { kind; expr; convert } -> Type.cast kind convert (type_of expr)
    | ArrayLength _ -> Type.u32
    | Relational _ -> Type.bool
    | Unary { expr; _ } | Derivative { expr; _ } -> type_of expr
    | ImageQuery { image; query } -> (
        match query with
        (* Handle image_query.Size with an image type *)
        | Size _ -> Type.image_dim (type_of image)
        (* Handle other image queries that return a scalar *)
        | NumLevels | NumLayers | NumSamples -> Type.u32)
    | ImageSample { image; _ } | ImageLoad { image; _ } ->
        Type.image_class (type_of image)
    | Swizzle { size; vector; _ } -> Type.swizzle size (type_of vector)
    | Splat { size; value } -> Type.splat size (type_of value)
    | Math { fun_; args } -> MathFunction.type_of fun_ (List.map type_of args)

  let rec to_string : t -> string = function
    | Literal l -> Literal.to_string l
    | ZeroValue ty -> Type.to_string ty ^ "()"
    | Compose { ty; components } ->
        let components =
          components |> List.map to_string |> String.concat ", "
        in
        Type.to_string ty ^ "(" ^ components ^ ")"
    | Access { base; index; location = _ } ->
        to_string base ^ "[" ^ to_string index ^ "]"
    | AccessIndex { base; index; location = _ } ->
        to_string base ^ "." ^ string_of_int index
    | Splat { size; value } ->
        "vec" ^ VectorSize.to_string size ^ "(" ^ to_string value ^ ")"
    | Swizzle { vector; indices; size; location = _ } ->
        let pattern =
          indices
          |> List.map (fun x -> VectorSize.nth_opt x size)
          |> List.map Option.get
        in
        to_string vector ^ "." ^ (pattern |> String.concat "")
    | Ident i -> Ident.name i
    | Load e ->
        Printf.sprintf "load(%s : %s)" (to_string e)
          (Type.to_string (type_of e))
    | ImageSample
        {
          image;
          sampler;
          gather;
          coordinate;
          array_index;
          offset;
          level;
          depth_ref;
        } ->
        let cmp = if Option.is_some depth_ref then "Compare" else "" in
        let lvl =
          match level with
          | Auto -> ""
          | Zero | Exact _ -> "Level"
          | Bias _ -> "Bias"
          | Gradient _ -> "Grad"
        in
        let args =
          (gather |> Option.map int |> Option.to_list)
          @ [ image; sampler; coordinate ]
          @ Option.to_list array_index @ Option.to_list depth_ref
          @ (match level with
            | Auto -> []
            | Zero -> [ float 0.0 ]
            | Exact e | Bias e -> [ e ]
            | Gradient { x; y } -> [ x; y ])
          @ Option.to_list offset
          |> List.map to_string |> String.concat ", "
        in
        Printf.sprintf "textureSample%s%s(%s)" cmp lvl args
    | ImageLoad { image; coordinate; array_index; sample; level } ->
        let args =
          let index =
            let index = Option.to_list sample @ Option.to_list level in
            Slice.from_start 1 |> Slice.sublist index
          in
          [ image; coordinate ] @ Option.to_list array_index @ index
          |> List.map to_string |> String.concat ", "
        in
        "textureLoad(" ^ args ^ ")"
    | ImageQuery { image; query } ->
        let func =
          match query with
          | Size _ -> "textureDimensions"
          | NumLevels -> "textureNumLevels"
          | NumLayers -> "textureNumLayers"
          | NumSamples -> "textureNumSamples"
        in
        let arg =
          match query with Size (Some e) -> ", " ^ to_string e | _ -> ""
        in
        func ^ "(" ^ to_string image ^ arg ^ ")"
    | Unary { op; expr } ->
        Printf.sprintf "%s(%s)" (UnaryOperator.to_string op) (to_string expr)
    | Binary b ->
        Printf.sprintf "(%s) %s (%s)" (to_string b.left)
          (BinaryOperator.to_string b.op)
          (to_string b.right)
    | Select _ -> (*TODO*) "Select"
    | Derivative { axis; ctrl; expr } ->
        Printf.sprintf "%s%s(%s)"
          (DerivativeAxis.to_string axis)
          (DerivativeControl.to_string ctrl)
          (to_string expr)
    | Relational { fun_; argument } ->
        Printf.sprintf "%s(%s)"
          (RelationalFunction.to_string fun_)
          (to_string argument)
    | Math { fun_; args } ->
        let args = args |> List.map to_string |> String.concat ", " in
        MathFunction.to_string fun_ ^ "(" ^ args ^ ")"
    | As { expr; kind; convert } ->
        let ty =
          match convert with
          | Some n -> Scalar.{ kind; width = n } |> Scalar.to_string
          | None -> ScalarKind.to_string kind
        in
        ty ^ "(" ^ to_string expr ^ ")"
    | ArrayLength e -> "arrayLength(" ^ to_string e ^ ")"

  (** [sample_level_to_string t] returns a string representation of the sample
      level [t].

      Example:
      {[
        sample_level_to_string (Exact expr)
        (* Output: "exact(<expression>)" *)
      ]} *)
  and sample_level_to_string : sample_level -> string = function
    | Auto -> "auto"
    | Zero -> "zero"
    | Exact expr -> Printf.sprintf "exact(%s)" (to_string expr)
    | Bias expr -> Printf.sprintf "bias(%s)" (to_string expr)
    | Gradient { x; y } ->
        Printf.sprintf "gradient(x: %s, y: %s)" (to_string x) (to_string y)

  (** [children ast] returns the list of child nodes for a given AST node [ast]
      of type [t]. This function is useful for traversing the abstract syntax
      tree (AST) of WGSL code, where each node may have zero or more children
      representing sub-expressions or components.

      For example, given a WGSL swizzle expression like `vec4(1.0).xyz`, the
      [Swizzle] node will contain a child node representing the vector being
      swizzled.

      This function simplifies working with nested WGSL constructs by extracting
      child nodes, enabling recursive tree traversals or transformations.

      @param ast the AST node to extract children from
      @return the list of child nodes contained within [ast] *)
  let children : t -> t list =
    let o_list : t option list -> t list = List.concat_map Option.to_list in
    function
    | Ident _ | Literal _ | ZeroValue _ -> []
    | Compose { components; _ } -> components
    | Access { base; index; _ } -> [ base; index ]
    | AccessIndex { base; _ } -> [ base ]
    | Splat { value; _ } -> [ value ]
    | Swizzle { vector; _ } -> [ vector ]
    | Load e -> [ e ]
    | ImageSample
        {
          image = e1;
          sampler = e2;
          coordinate = e3;
          array_index = o1;
          offset = o2;
          depth_ref = o3;
          gather = _;
          level;
        } ->
        let level =
          match level with
          | Auto | Zero -> []
          | Bias e | Exact e -> [ e ]
          | Gradient { x; y } -> [ x; y ]
        in
        [ e1; e2; e3 ] @ o_list [ o1; o2 ] @ level @ o_list [ o3 ]
    | ImageLoad
        {
          image = e1;
          coordinate = e2;
          array_index = o1;
          sample = o2;
          level = o3;
        } ->
        [ e1; e2 ] @ o_list [ o1; o2; o3 ]
    | ImageQuery { image = e; query = q } ->
        let o = match q with Size o -> o | _ -> None in
        e :: Option.to_list o
    | Unary { expr; _ } -> [ expr ]
    | Binary { left; right; _ } -> [ left; right ]
    | Select { condition; accept; reject } -> [ condition; accept; reject ]
    | Derivative { expr; _ } -> [ expr ]
    | Relational { argument; _ } -> [ argument ]
    | Math { fun_ = _; args } -> args
    | As { expr; _ } -> [ expr ]
    | ArrayLength e -> [ e ]

  (** Map applied to child expressions. *)
  let map (f : t -> t) : t -> t =
   fun e ->
    match e with
    | Ident _ | Literal _ | ZeroValue _ -> e
    | Compose { components; ty } ->
        Compose { components = List.map f components; ty }
    | Access { base; index; location } ->
        Access { base = f base; index = f index; location }
    | AccessIndex { base; index; location } ->
        AccessIndex { base = f base; index; location }
    | Splat { value; size } -> Splat { value = f value; size }
    | Swizzle { vector; size; indices; location } ->
        Swizzle { vector = f vector; size; indices; location }
    | Load e -> Load (f e)
    | ImageSample
        {
          image;
          sampler;
          coordinate;
          array_index;
          offset;
          depth_ref;
          gather;
          level;
        } ->
        ImageSample
          {
            image = f image;
            sampler = f sampler;
            coordinate = f coordinate;
            array_index = Option.map f array_index;
            offset = Option.map f offset;
            depth_ref = Option.map f depth_ref;
            gather;
            level;
          }
    | ImageLoad { image; coordinate; array_index; sample; level } ->
        ImageLoad
          {
            image = f image;
            coordinate = f coordinate;
            array_index = Option.map f array_index;
            sample = Option.map f sample;
            level = Option.map f level;
          }
    | ImageQuery { image; query } ->
        let query =
          match query with Size o -> Size (Option.map f o) | _ -> query
        in
        ImageQuery { image = f image; query }
    | Unary { expr; op } -> Unary { expr = f expr; op }
    | Binary { left; right; op } ->
        Binary { left = f left; right = f right; op }
    | Select { condition; accept; reject } ->
        Select { condition = f condition; accept = f accept; reject = f reject }
    | Derivative { expr; axis; ctrl } ->
        Derivative { expr = f expr; axis; ctrl }
    | Relational { argument; fun_ } ->
        Relational { argument = f argument; fun_ }
    | Math { fun_; args } -> Math { fun_; args = List.map f args }
    | As { expr; kind; convert } -> As { expr = f expr; kind; convert }
    | ArrayLength e -> ArrayLength (f e)

  let rec access_index ?(location = Location.empty) (index : int) (base : t) : t
      =
    let default = AccessIndex { base; index; location } in
    match base with
    | Compose { components; _ } ->
        List.nth_opt components index |> Option.value ~default
    | Swizzle { indices; vector; _ } -> (
        (* when we find a swizzle, we must translate the index,
         and access with the new index *)
        match List.nth_opt indices index with
        | Some index -> access_index ~location index vector
        | None -> default)
    | _ -> AccessIndex { base; index; location }

  (** When the type of the expression is a vector or a matrix, return the
      components *)
  let to_vector (e : t) : (t list * Type.t) option =
    let open Stage0 in
    let ty = type_of e in
    match ty.inner with
    | Vector { size; _ } ->
        Some
          ( Py.range (VectorSize.to_int size)
            |> List.map (fun i -> access_index i e),
            ty )
    | _ -> None

  let true_ : t = Literal Literal.true_
  let false_ : t = Literal Literal.false_

  let and_ : t -> t -> t =
   fun left right -> Binary { op = LogicalAnd; left; right }

  let or_ : t -> t -> t =
   fun left right -> Binary { op = LogicalOr; left; right }

  let all (l : t list) : t =
    match l with [] -> true_ | x :: l -> List.fold_left and_ x l

  let any (l : t list) : t =
    match l with [] -> false_ | x :: l -> List.fold_left or_ x l

  let rec parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Literal" ->
        let* value = with_field "value" Literal.parse o in
        Ok (Literal value)
    | "ZeroValue" ->
        let* ty = with_field "ty" Type.parse o in
        Ok (ZeroValue ty)
    | "Compose" ->
        let* ty = with_field "ty" Type.parse o in
        let* components = with_field "components" (cast_map parse) o in
        Ok (Compose { ty; components })
    | "Access" ->
        let* base = with_field "base" parse o in
        let* index = with_field "index" parse o in
        let* location = with_field "location" parse_location o in
        Ok (Access { base; index; location })
    | "AccessIndex" ->
        let* base = with_field "base" parse o in
        let* index = with_field "index" cast_int o in
        let* location = with_field "location" parse_location o in
        Ok (AccessIndex { base; index; location })
    | "Splat" ->
        let* value = with_field "value" parse o in
        let* size = with_field "size" VectorSize.parse o in
        Ok (Splat { value; size })
    | "Swizzle" ->
        let* size = with_field "size" VectorSize.parse o in
        let* vector = with_field "vector" parse o in
        let* indices = with_field "indices" (cast_map cast_int) o in
        let* location = with_field "location" parse_location o in
        Ok (Swizzle { size; vector; indices; location })
    | _ when IdentKind.is_kind kind ->
        let* i = Ident.parse j in
        Ok (Ident i)
    | "Load" ->
        let* value = with_field "pointer" parse o in
        Ok (Load value)
    | "ImageSample" ->
        let* image = with_field "image" parse o in
        let* sampler = with_field "sampler" parse o in
        let* gather = with_field "gather" (cast_option cast_int) o in
        let* coordinate = with_field "coordinate" parse o in
        let* array_index = with_field "array_index" (cast_option parse) o in
        let* offset = with_field "offset" (cast_option parse) o in
        let* level = with_field "level" sample_level_parse o in
        let* depth_ref = with_field "depth_ref" (cast_option parse) o in
        Ok
          (ImageSample
             {
               image;
               sampler;
               gather;
               coordinate;
               array_index;
               offset;
               level;
               depth_ref;
             })
    | "ImageLoad" ->
        let* image = with_field "image" parse o in
        let* coordinate = with_field "coordinate" parse o in
        let* array_index = with_field "array_index" (cast_option parse) o in
        let* sample = with_field "sample" (cast_option parse) o in
        let* level = with_field "level" (cast_option parse) o in
        Ok (ImageLoad { image; coordinate; array_index; sample; level })
    | "ImageQuery" ->
        let* image = with_field "image" parse o in
        let* query = with_field "query" image_query_parse o in
        Ok (ImageQuery { query; image })
    | "Unary" ->
        let* op = with_field "op" UnaryOperator.parse o in
        let* expr = with_field "expr" parse o in
        Ok (Unary { op; expr })
    | "Binary" ->
        let* left = with_field "left" parse o in
        let* right = with_field "right" parse o in
        let* op = with_field "op" BinaryOperator.parse o in
        Ok (Binary { op; left; right })
    | "Select" ->
        let* condition = with_field "condition" parse o in
        let* accept = with_field "accept" parse o in
        let* reject = with_field "reject" parse o in
        Ok (Select { condition; accept; reject })
    | "Relational" ->
        let* argument = with_field "argument" parse o in
        let* fun_ = with_field "fun" RelationalFunction.parse o in
        Ok (Relational { fun_; argument })
    | "Math" ->
        let* fun_ = with_field "fun" MathFunction.parse o in
        let* args = with_field "args" (cast_map parse) o in
        Ok (Math { fun_; args })
    | "As" ->
        let* expr = with_field "expr" parse o in
        let* kind = with_field "scalar_kind" ScalarKind.parse o in
        let* convert = with_field "convert" (cast_option cast_int) o in
        Ok (As { expr; kind; convert })
    | "ArrayLength" ->
        let* e = with_field "array" parse o in
        Ok (ArrayLength e)
    | "Derivative" ->
        let* axis = with_field "axis" DerivativeAxis.parse o in
        let* ctrl = with_field "ctrl" DerivativeControl.parse o in
        let* expr = with_field "expr" parse o in
        Ok (Derivative { axis; ctrl; expr })
    | _ -> failwith kind

  and image_query_parse (j : json) : image_query j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Size" ->
        let* e = with_field "level" (cast_option parse) o in
        Ok (Size e)
    | "NumLevels" -> Ok NumLevels
    | "NumLayers" -> Ok NumLayers
    | "NumSamples" -> Ok NumSamples
    | _ -> root_cause "parse_image_query" j

  (** [sample_level_parse j] parses a JSON object [j] into a sample level of
      type [sample_level].

      This function expects the JSON to contain a field [kind] indicating the
      type of sample level, with optional fields [value], [x], and [y] depending
      on the kind.

      Example JSON:
      {[
        { "kind": "Exact", "value": <expression> }
      ]}

      @param j The JSON object to parse.
      @return A result containing the parsed sample level or an error. *)
  and sample_level_parse (j : json) : sample_level j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Auto" -> Ok Auto
    | "Zero" -> Ok Zero
    | "Exact" ->
        let* expr = with_field "value" parse o in
        Ok (Exact expr)
    | "Bias" ->
        let* expr = with_field "value" parse o in
        Ok (Bias expr)
    | "Gradient" ->
        let* x = with_field "x" parse o in
        let* y = with_field "y" parse o in
        Ok (Gradient { x; y })
    | _ -> root_cause "sample_level" j

  let zero (s : Scalar.t) : t = Literal (Literal.zero s)

  let compose (ty : Type.t) (components : t list) : t =
    match ty.inner with
    | Vector { size; scalar } ->
        let components =
          match (size, components, List.map type_of components) with
          | _, [ e ], [ ty ] when Type.is_vector ty ->
              (* Vectors accept a copy-constructor *)
              size |> VectorSize.to_int |> Py.range
              |> List.map (fun i -> access_index i e)
          | _, [ e ], [ ty ] when Type.is_scalar ty ->
              (* Vectors accept a component-wise initialization
            when argument is a scalar. *)
              repeat e (VectorSize.to_int size)
          | _, [], _ ->
              let z = zero scalar in
              repeat z (VectorSize.to_int size)
          | _, c, _ when List.length components < VectorSize.to_int size ->
              (* when there are fewer arguments than the size of the vector, then
            we must expand each vector component *)
              c
              |> List.concat_map (fun e ->
                     match to_vector e with Some (l, _) -> l | None -> [ e ])
          | _, _, _ -> components
        in
        Compose { ty; components }
    | _ -> Compose { ty; components }

  let rec simplify : t -> t = function
    | Splat { size; value } -> (
        let value = simplify value in
        let ty = type_of value in
        match Type.to_scalar ty with
        | Some scalar -> compose (Type.make (Vector { size; scalar })) [ value ]
        | None -> Splat { size; value })
    | Compose { ty; components } -> compose ty (List.map simplify components)
    (* Eval if possible *)
    | Access { base; index = Literal l; location } ->
        let index = Literal.to_int l |> Option.get in
        simplify (AccessIndex { base; index; location })
    (* Eval if possible *)
    | AccessIndex { base; index; location } -> (
        let base = simplify base in
        let e : t = AccessIndex { base; index; location } in
        match base with
        | Compose { components; _ } ->
            List.nth_opt components index |> Option.value ~default:e
        | _ -> e)
    (* Inline component-wise *)
    | Relational { fun_ = All; argument = a } -> (
        let a = simplify a in
        match to_vector a with Some (a, _) -> all a | None -> a)
    (* Inline component-wise *)
    | Relational { fun_ = Any; argument = a } -> (
        let a = simplify a in
        match to_vector a with Some (a, _) -> any a | None -> a)
    (* Inline component-wise *)
    | Binary { op; left = l; right = r } as e -> (
        match (to_vector l, to_vector r) with
        | Some (l, l_ty), Some (r, r_ty) ->
            let ty = BinaryOperator.type_of op l_ty r_ty in
            let components =
              Stage0.Common.zip (List.map simplify l) (List.map simplify r)
              |> List.map (fun (left, right) -> Binary { op; left; right })
            in
            Compose { ty; components }
        | _, _ -> map simplify e)
    | Unary { op; expr = e } -> (
        match to_vector e with
        | Some (l, ty) ->
            let components =
              l |> List.map (fun e -> Unary { op; expr = simplify e })
            in
            Compose { ty; components }
        | None -> Unary { op; expr = simplify e })
    (* Inline component-wise *)
    | Select { condition = c; accept = a; reject = r } as e -> (
        match (to_vector c, to_vector a, to_vector r) with
        | Some (c, _), Some (a, ty), Some (r, _) ->
            let components : t list =
              Stage0.Common.zip3 (List.map simplify c) (List.map simplify a)
                (List.map simplify r)
              |> List.map (fun (c, a, r) : t ->
                     Select { condition = c; accept = a; reject = r })
            in
            Compose { ty; components }
        | _, _, _ -> map simplify e)
    (* continue simplifying *)
    | e -> map simplify e
end

module LocalDeclaration = struct
  type t = { var : Variable.t; init : Expression.t option; ty : Type.t }

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* var = parse_var o in
    let* init = with_field "init" (cast_option Expression.parse) o in
    let* ty = with_field "ty" Type.parse o in
    Ok { var; init; ty }

  let to_string (d : t) : string =
    let init =
      match d.init with
      | Some init -> " = " ^ Expression.to_string init
      | None -> ""
    in
    Variable.name d.var ^ " : " ^ Type.to_string d.ty ^ init
end

module AtomicFunction = struct
  type t =
    | Add
    | Subtract
    | And
    | ExclusiveOr
    | InclusiveOr
    | Min
    | Max
    | Exchange of { compare : Expression.t option }

  let to_string : t -> string = function
    | Add -> "Add"
    | Subtract -> "Sub"
    | And -> "And"
    | ExclusiveOr -> "Xor"
    | InclusiveOr -> "Or"
    | Min -> "Min"
    | Max -> "Max"
    | Exchange { compare = None } -> "Exchange"
    | Exchange _ -> "ExchangeWeak"

  let to_list : t -> Expression.t list = function
    | Exchange { compare } -> Option.to_list compare
    | Add | Subtract | ExclusiveOr | InclusiveOr | And | Min | Max -> []

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Add" -> Ok Add
    | "Subtract" -> Ok Subtract
    | "And" -> Ok And
    | "ExclusiveOr" -> Ok ExclusiveOr
    | "InclusiveOr" -> Ok InclusiveOr
    | "Min" -> Ok Min
    | "Max" -> Ok Max
    | "Exchange" ->
        let* c = with_field "condition" (cast_option Expression.parse) o in
        Ok (Exchange { compare = c })
    | _ -> root_cause ("AtomicFunction.parse: unknown kind: " ^ kind) j
end

module GatherMode = struct
  type t =
    | BroadcastFirst
    | Broadcast of Expression.t
    | Shuffle of Expression.t
    | ShuffleDown of Expression.t
    | ShuffleUp of Expression.t
    | ShuffleXor of Expression.t

  let to_string : t -> string = function
    | BroadcastFirst -> "broadcast_first"
    | Broadcast expr ->
        Printf.sprintf "broadcast(%s)" (Expression.to_string expr)
    | Shuffle expr -> Printf.sprintf "shuffle(%s)" (Expression.to_string expr)
    | ShuffleDown expr ->
        Printf.sprintf "shuffle_down(%s)" (Expression.to_string expr)
    | ShuffleUp expr ->
        Printf.sprintf "shuffle_up(%s)" (Expression.to_string expr)
    | ShuffleXor expr ->
        Printf.sprintf "shuffle_xor(%s)" (Expression.to_string expr)

  let expression : t -> Expression.t option = function
    | BroadcastFirst -> None
    | Broadcast e | Shuffle e | ShuffleDown e | ShuffleUp e | ShuffleXor e ->
        Some e

  let name : t -> string = function
    | BroadcastFirst -> "BroadcastFirst"
    | Broadcast _ -> "Broadcast"
    | Shuffle _ -> "Shuffle"
    | ShuffleDown _ -> "ShuffleDown"
    | ShuffleUp _ -> "ShuffleUp"
    | ShuffleXor _ -> "ShuffleXor"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "BroadcastFirst" -> Ok BroadcastFirst
    | "Broadcast" ->
        let* expr = with_field "value" Expression.parse o in
        Ok (Broadcast expr)
    | "Shuffle" ->
        let* expr = with_field "value" Expression.parse o in
        Ok (Shuffle expr)
    | "ShuffleDown" ->
        let* expr = with_field "value" Expression.parse o in
        Ok (ShuffleDown expr)
    | "ShuffleUp" ->
        let* expr = with_field "value" Expression.parse o in
        Ok (ShuffleUp expr)
    | "ShuffleXor" ->
        let* expr = with_field "value" Expression.parse o in
        Ok (ShuffleXor expr)
    | _ -> root_cause "GatherMode" j
end

module CollectiveOperation = struct
  type t = Reduce | InclusiveScan | ExclusiveScan

  let to_string : t -> string = function
    | Reduce -> "reduce"
    | InclusiveScan -> "inclusive_scan"
    | ExclusiveScan -> "exclusive_scan"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Reduce" -> Ok Reduce
    | "InclusiveScan" -> Ok InclusiveScan
    | "ExclusiveScan" -> Ok ExclusiveScan
    | _ -> root_cause "CollectiveOperation" j
end

module SubgroupOperation = struct
  type t = All | Any | Add | Mul | Min | Max | And | Or | Xor

  let to_string : t -> string = function
    | All -> "all"
    | Any -> "any"
    | Add -> "add"
    | Mul -> "mul"
    | Min -> "min"
    | Max -> "max"
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "All" -> Ok All
    | "Any" -> Ok Any
    | "Add" -> Ok Add
    | "Mul" -> Ok Mul
    | "Min" -> Ok Min
    | "Max" -> Ok Max
    | "And" -> Ok And
    | "Or" -> Ok Or
    | "Xor" -> Ok Xor
    | _ -> root_cause "SubgroupOperation" j
end

(** {1 SwitchValue Module}

    This module defines a type [t] to represent values in a WGSL switch
    statement. In WGSL, [switch] statements can match against 32-bit signed or
    unsigned integer literals or use a [default] case.

    This module provides utilities to convert these switch values to strings and
    parse them from JSON representations. These utilities can be useful when
    working with AST representations of WGSL.

    Example WGSL switch:
    {[
      switch (value) {
        case 1: { /*...*/ }
        case 42u: { /*...*/ }
        default: { /*...*/ }
      }
    ]} *)

module SwitchValue = struct
  (** {2 Type Definition}
      [t] is a type representing the possible values of a WGSL switch case. *)
  type t =
    | I32 of int  (** A 32-bit signed integer value. *)
    | U32 of int
        (** A 32-bit unsigned integer value (using OCaml's [int32]). *)
    | Default  (** The [default] case in a WGSL switch statement. *)

  (** Converts a [SwitchValue.t] to its string representation.

      @param value the switch value to convert
      @return a string representation of the switch value

      Example:
      {[
        let s = SwitchValue.to_string (I32 42l)
        (* s = "i32(42)" *)

        let s = SwitchValue.to_string (U32 42l)
        (* s = "u32(42)" *)

        let s = SwitchValue.to_string Default
        (* s = "default" *)
      ]} *)
  let to_string : t -> string = function
    | I32 value -> Printf.sprintf "i32(%d)" value
    | U32 value -> Printf.sprintf "u32(%d)" value
    | Default -> "default"

  (** Parses a [SwitchValue.t] from a JSON object.

      @param j the JSON representation of a switch value
      @return a result containing either a parsed [SwitchValue.t] or an error

      Example JSON input:
      {[
        { "kind": "I32", "value": 42 }
      ]}
      {[
        { "kind": "U32", "value": 42 }
      ]}
      {[
        { "kind": "Default" }
      ]}

      WGSL Example:
      {[
        let json = `Assoc [("kind", `String "I32"); ("value", `Int 42)] in
        let result = SwitchValue.parse json
        (* result = Ok (I32 42l) *)
      ]} *)
  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "I32" ->
        let* value = with_field "value" cast_int o in
        Ok (I32 value)
    | "U32" ->
        let* value = with_field "value" cast_int o in
        Ok (U32 value)
    | "Default" -> Ok Default
    | _ -> root_cause "SwitchValue" j
end

module Statement = struct
  type t =
    | Block of t list
    | If of { condition : Expression.t; accept : t list; reject : t list }
    | Switch of { selector : Expression.t; cases : switch_case list }
    | Loop of {
        body : t list;
        continuing : t list;
        break_if : Expression.t option;
      }
    | Break
    | Continue
    | Return of Expression.t option
    | Kill
    | Barrier of { storage : bool; work_group : bool; sub_group : bool }
    | Store of { pointer : Expression.t; value : Expression.t }
    | ImageStore of {
        image : Expression.t;
        coordinate : Expression.t;
        array_index : Expression.t option;
        value : Expression.t;
      }
    | Atomic of {
        pointer : Expression.t;
        fun_ : AtomicFunction.t;
        value : Expression.t;
        result : Ident.t option;
        location : Location.t;
      }
    | WorkGroupUniformLoad of { pointer : Expression.t; result : Ident.t }
    | Call of {
        function_ : string;
        arguments : Expression.t list;
        result : Ident.t option;
      }
    | SubgroupBallot of { result : Ident.t; predicate : Expression.t option }
    | SubgroupGather of {
        mode : GatherMode.t;
        argument : Expression.t;
        result : Ident.t;
      }
    | SubgroupCollectiveOperation of {
        op : SubgroupOperation.t;
        collective_op : CollectiveOperation.t;
        argument : Expression.t;
        result : Ident.t;
      }

  and switch_case = {
    value : SwitchValue.t;
        (** The value to match against the switch expression. *)
    body : t list;  (** The block of code executed if the value matches. *)
    fall_through : bool;  (** Whether control should proceed to the next case. *)
  }
  (** The type representing a switch case in WGSL. A case includes:
      - [value]: The case value to match against the switch expression.
      - [body]: A block of code to execute if the case matches.
      - [fall_through]: A boolean flag indicating whether execution should fall
        through to the next case (if [true]), similar to the 'fall-through'
        behavior in C-like languages. *)

  (** Returns the child expressions of the current statement. Does not recurse
      to child statetements. *)
  let expressions : t -> Expression.t list = function
    | Call { arguments = l; _ } -> l
    | ImageStore { image = e1; coordinate = e2; array_index = o3; value = e4 }
      ->
        [ e1; e2 ] @ Option.to_list o3 @ [ e4 ]
    | Store { pointer = e1; value = e2 } -> [ e1; e2 ]
    | SubgroupBallot { predicate = o; _ } | Return o | Loop { break_if = o; _ }
      ->
        Option.to_list o
    | SubgroupCollectiveOperation { argument = e; _ }
    | SubgroupGather { argument = e; _ }
    | WorkGroupUniformLoad { pointer = e; _ }
    | Atomic { pointer = e; _ }
    | Switch { selector = e; _ }
    | If { condition = e; _ } ->
        [ e ]
    | Continue | Break | Block _ | Kill | Barrier _ -> []

  (** Update every expression that occurs in a statement *)
  let map (f : Expression.t -> Expression.t) : t -> t =
    let rec map : t -> t = function
      | Block l -> Block (List.map map l)
      | If { condition = c; accept = a; reject = r } ->
          If
            {
              condition = f c;
              accept = List.map map a;
              reject = List.map map r;
            }
      | Switch { selector = s; cases } ->
          let cases =
            cases
            |> List.map (function { value; body; fall_through } ->
                   { value; body = List.map map body; fall_through })
          in
          Switch { selector = f s; cases }
      | Loop { body; continuing; break_if } ->
          Loop
            {
              body = List.map map body;
              continuing = List.map map continuing;
              break_if = Option.map f break_if;
            }
      | Return e -> Return (Option.map f e)
      | Store { pointer; value } ->
          Store { pointer = f pointer; value = f value }
      | ImageStore { image; coordinate; array_index; value } ->
          ImageStore
            {
              image = f image;
              coordinate = f coordinate;
              array_index = Option.map f array_index;
              value = f value;
            }
      | Atomic { pointer; fun_; value; result; location } ->
          Atomic
            { pointer = f pointer; fun_; value = f value; result; location }
      | WorkGroupUniformLoad { pointer; result } ->
          WorkGroupUniformLoad { pointer = f pointer; result }
      | Call { function_; arguments; result } ->
          Call { function_; arguments = List.map f arguments; result }
      | SubgroupBallot { result; predicate } ->
          SubgroupBallot { result; predicate = Option.map f predicate }
      | SubgroupGather { mode; argument; result } ->
          SubgroupGather { mode; argument = f argument; result }
      | SubgroupCollectiveOperation { op; collective_op; argument; result } ->
          SubgroupCollectiveOperation
            { op; collective_op; argument = f argument; result }
      | s -> s
    in
    map

  let rec parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "Block" ->
        let* l = with_field "body" (cast_map parse) o in
        Ok (Block l)
    | "If" ->
        let* condition = with_field "condition" Expression.parse o in
        let* accept = with_field "accept" (cast_map parse) o in
        let* reject = with_field "reject" (cast_map parse) o in
        Ok (If { accept; reject; condition })
    | "Switch" ->
        let* selector = with_field "selector" Expression.parse o in
        let* cases = with_field "cases" (cast_map switch_case_parse) o in
        Ok (Switch { selector; cases })
    | "Loop" ->
        let* body = with_field "body" (cast_map parse) o in
        let* continuing = with_field "continuing" (cast_map parse) o in
        let* break_if =
          with_field "break_if" (cast_option Expression.parse) o
        in
        Ok (Loop { body; continuing; break_if })
    | "Break" -> Ok Break
    | "Continue" -> Ok Continue
    | "Return" ->
        let* e = with_field "value" (cast_option Expression.parse) o in
        Ok (Return e)
    | "Kill" -> Ok Kill
    | "Barrier" ->
        let* b = with_field "value" cast_object o in
        let* storage = with_field "storage" cast_bool b in
        let* work_group = with_field "work_group" cast_bool b in
        let* sub_group = with_field "sub_group" cast_bool b in
        Ok (Barrier { storage; work_group; sub_group })
    | "Store" ->
        let* pointer = with_field "pointer" Expression.parse o in
        let* value = with_field "value" Expression.parse o in
        Ok (Store { pointer; value })
    | "ImageStore" ->
        let* image = with_field "image" Expression.parse o in
        let* coordinate = with_field "coordinate" Expression.parse o in
        let* array_index =
          with_field "array_index" (cast_option Expression.parse) o
        in
        let* value = with_field "value" Expression.parse o in
        Ok (ImageStore { image; coordinate; array_index; value })
    | "Atomic" ->
        let* pointer = with_field "pointer" Expression.parse o in
        let* fun_ = with_field "fun" AtomicFunction.parse o in
        let* value = with_field "value" Expression.parse o in
        let* result = with_field "result" (cast_option Ident.parse) o in
        let* location = with_field "location" parse_location o in
        Ok (Atomic { pointer; fun_; value; result; location })
    | "WorkGroupUniformLoad" ->
        let* pointer = with_field "pointer" Expression.parse o in
        let* result = with_field "result" Ident.parse o in
        Ok (WorkGroupUniformLoad { pointer; result })
    | "Call" ->
        let* function_ = with_field "function" cast_string o in
        let* arguments = with_field "arguments" (cast_map Expression.parse) o in
        let* result = with_field "result" (cast_option Ident.parse) o in
        Ok (Call { function_; arguments; result })
    | "SubgroupBallot" ->
        let* predicate =
          with_field "predicate" (cast_option Expression.parse) o
        in
        let* result = with_field "result" Ident.parse o in
        Ok (SubgroupBallot { predicate; result })
    | "SubgroupGather" ->
        let* mode = with_field "mode" GatherMode.parse o in
        let* argument = with_field "argument" Expression.parse o in
        let* result = with_field "result" Ident.parse o in
        Ok (SubgroupGather { mode; argument; result })
    | "SubgroupCollectiveOperation" ->
        let* op = with_field "op" SubgroupOperation.parse o in
        let* collective_op =
          with_field "collective_op" CollectiveOperation.parse o
        in
        let* argument = with_field "argument" Expression.parse o in
        let* result = with_field "result" Ident.parse o in
        Ok (SubgroupCollectiveOperation { op; collective_op; argument; result })
    | _ -> root_cause ("Statement.parse: unknown kind: " ^ kind) j

  (** Parses a JSON representation of a switch case into a [t] type.

      Example JSON input:
      {[
        {
          "value": 1,
          "body": [],
          "fall_through": false
        }
      ]}

      @param j The JSON object representing a switch case.
      @return A parsed switch case or an error if the JSON is invalid. *)
  and switch_case_parse (j : json) : switch_case j_result =
    let open Rjson in
    let* o = cast_object j in
    let* value = with_field "value" SwitchValue.parse o in
    let* body = with_field "body" (cast_map parse) o in
    let* fall_through = with_field "fall_through" cast_bool o in
    Ok { value; body; fall_through }

  let rec to_s : t -> Indent.t list = function
    | Block l -> [ Line "{"; Block (block_to_s l); Line "}" ]
    | If { accept; reject; condition } ->
        let open Indent in
        [
          Line ("if (" ^ Expression.to_string condition ^ ") {");
          Block (block_to_s accept);
        ]
        @ (if reject = [] then []
           else [ Line "} else {"; Block (block_to_s reject) ])
        @ [ Line "}" ]
    | Switch { selector; cases } ->
        [ Indent.Line ("switch (" ^ Expression.to_string selector ^ ") {") ]
        @ List.concat_map switch_case_to_s cases
        @ [ Line "}" ]
    | Loop { body; continuing; break_if } ->
        [
          Line "loop {";
          Block (block_to_s body);
          Line "} continuing {";
          Block
            (block_to_s continuing
            @
            match break_if with
            | Some b -> [ Line ("break if " ^ Expression.to_string b ^ ";") ]
            | None -> []);
          Line "}";
        ]
    | Break -> [ Line "break;" ]
    | Continue -> [ Line "continue;" ]
    | Return None -> [ Line "return;" ]
    | Return (Some e) -> [ Line ("return " ^ Expression.to_string e ^ ";") ]
    | Kill -> [ Line "kill;" ]
    | Barrier b ->
        if b.storage then [ Line "storageBarrier();" ]
        else
          []
          @
          if b.work_group then [ Indent.Line "workgroupBarrier();" ]
          else
            []
            @ if b.sub_group then [ Indent.Line "subgroupBarrier();" ] else []
    | Store { pointer; value } ->
        let line =
          Printf.sprintf "%s = %s;"
            (Expression.to_string pointer)
            (Expression.to_string value)
        in
        [ Line line ]
    | ImageStore { image; coordinate; array_index; value } ->
        let args =
          [ image; coordinate ] @ Option.to_list array_index @ [ value ]
          |> List.map Expression.to_string
          |> String.concat ", "
        in
        [ Line (Printf.sprintf "textureStore(%s);" args) ]
    | Atomic { pointer; fun_; value; result; location = _ } ->
        let target =
          match result with
          | Some x -> Printf.sprintf "let %s = " (Ident.to_string x)
          | None -> ""
        in
        let args =
          [ pointer ] @ AtomicFunction.to_list fun_ @ [ value ]
          |> List.map Expression.to_string
          |> String.concat ", "
        in
        let line =
          Printf.sprintf "%satomic%s(%s);" target
            (AtomicFunction.to_string fun_)
            args
        in
        [ Line line ]
    | WorkGroupUniformLoad { pointer; result } ->
        let line =
          Printf.sprintf "let %s = workgroupUniformLoad(%s);"
            (Ident.to_string result)
            (Expression.to_string pointer)
        in
        [ Line line ]
    | Call { function_; arguments; result } ->
        let result =
          result
          |> Option.map (fun e -> "let " ^ Ident.to_string e ^ " = ")
          |> Option.value ~default:""
        in
        let arguments =
          arguments |> List.map Expression.to_string |> String.concat ", "
        in
        [ Line (result ^ function_ ^ "(" ^ arguments ^ ");") ]
    | SubgroupBallot { result; predicate } ->
        let arg =
          predicate
          |> Option.map Expression.to_string
          |> Option.value ~default:""
        in
        let line =
          Printf.sprintf "let %s = subgroupBallot(%s);" (Ident.to_string result)
            arg
        in
        [ Line line ]
    | SubgroupGather { mode; argument; result } ->
        let line =
          let args =
            argument :: (GatherMode.expression mode |> Option.to_list)
            |> List.map Expression.to_string
            |> String.concat ", "
          in
          Printf.sprintf "let %s = subgroup%s(%s);" (Ident.to_string result)
            (GatherMode.name mode) args
        in
        [ Line line ]
    | SubgroupCollectiveOperation { op; collective_op = _; argument; result } ->
        let line =
          Printf.sprintf "let %s: %s = subgroup%s(%s);" (Ident.to_string result)
            (Type.to_string result.ty)
            (SubgroupOperation.to_string op)
            (Expression.to_string argument)
        in
        [ Line line ]

  and block_to_s l = List.concat_map to_s l

  and switch_case_to_s (s : switch_case) : Indent.t list =
    [
      Line ("case " ^ SwitchValue.to_string s.value ^ ":");
      Block (block_to_s (s.body @ if s.fall_through then [ Break ] else []));
    ]

  let to_string (s : t) : string = Indent.to_string (to_s s)
end

module Function = struct
  type t = {
    name : string;
    arguments : FunctionArgument.t list;
    result : FunctionResult.t option;
    locals : LocalDeclaration.t list;
    body : Statement.t list;
  }

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    (* XXX: in Naga this is an optional string *)
    let* result = with_field "result" (cast_option FunctionResult.parse) o in
    let* arguments =
      with_field "arguments" (cast_map FunctionArgument.parse) o
    in
    let* locals = with_field "locals" (cast_map LocalDeclaration.parse) o in
    let* body = with_field "body" (cast_map Statement.parse) o in
    Ok { name; result; arguments; locals; body }

  let map (f : Statement.t -> Statement.t) (b : t) : t =
    { b with body = List.map f b.body }

  let to_string (e : t) : string = e.name

  let to_s (f : t) : Indent.t list =
    let args =
      f.arguments |> List.map FunctionArgument.to_string |> String.concat ", "
    in
    let locals : string =
      f.locals |> List.map LocalDeclaration.to_string |> String.concat ", "
    in
    [
      Line "";
      Line ("fn " ^ f.name ^ "(" ^ args ^ ") {");
      Block
        ((if locals = "" then [] else [ Indent.Line ("var " ^ locals ^ ";") ])
        @ Statement.block_to_s f.body);
      Line "}";
    ]
end

module ShaderStage = struct
  type t = Vertex | Fragment | Compute

  let to_string : t -> string = function
    | Vertex -> "vertex"
    | Fragment -> "fragment"
    | Compute -> "compute"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* name = cast_string j in
    match name with
    | "Vertex" -> Ok Vertex
    | "Fragment" -> Ok Fragment
    | "Compute" -> Ok Compute
    | _ -> root_cause ("ShaderStage.parse: unknown name: " ^ name) j
end

module EntryPoint = struct
  type t = {
    name : string;
    function_ : Function.t;
    workgroup_size : Dim3.t;
    (*
    TODO: early_depth_test: EarlyDepthTest.t option;
    *)
    stage : ShaderStage.t;
  }

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* stage = with_field "stage" ShaderStage.parse o in
    let* workgroup_size =
      with_field "workgroup_size"
        (fun j ->
          match Dim3.from_json j with Ok d -> Ok d | Error e -> root_cause e j)
        o
    in
    let* function_ = with_field "function" Function.parse o in
    Ok { name; workgroup_size; stage; function_ }

  let map (f : Statement.t -> Statement.t) (e : t) : t =
    { e with function_ = Function.map f e.function_ }

  let to_string (e : t) : string = e.name

  let to_s (d : t) : Indent.t list =
    let args =
      d.function_.arguments
      |> List.map FunctionArgument.to_string
      |> String.concat ", "
    in
    let locals : string =
      d.function_.locals
      |> List.map LocalDeclaration.to_string
      |> String.concat ", "
    in
    [
      Line "";
      Line
        ("@"
        ^ ShaderStage.to_string d.stage
        ^ " " ^ "@workgroup_size("
        ^ Dim3.to_string d.workgroup_size
        ^ ") " ^ "fn " ^ d.name ^ "(" ^ args ^ ") {");
      Block
        ((if locals = "" then [] else [ Indent.Line ("var " ^ locals ^ ";") ])
        @ Statement.block_to_s d.function_.body);
      Line "}";
    ]
end

module ResourceBinding = struct
  type t = { group : int; binding : int }

  let to_string (r : t) : string =
    "@group(" ^ string_of_int r.group ^ ") @binding(" ^ string_of_int r.binding
    ^ ")"

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* group = with_field "group" cast_int o in
    let* binding = with_field "binding" cast_int o in
    Ok { group; binding }
end

module DeclarationKind = struct
  type t =
    | GlobalVariable of {
        space : AddressSpace.t;
        binding : ResourceBinding.t option;
      }
    | Constant
    | Override of { id : int option }

  let is_kind : string -> bool = function
    | "GlobalDeclaration" | "ConstDeclaration" | "Constant" -> true
    | _ -> false

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | "GlobalDeclaration" ->
        let* space = with_field "space" AddressSpace.parse o in
        let* binding =
          with_field "binding" (cast_option ResourceBinding.parse) o
        in
        Ok (GlobalVariable { space; binding })
    | "ConstDeclaration" -> Ok Constant
    | "Override" ->
        let* id = with_field "space" (cast_option cast_int) o in
        Ok (Override { id })
    | _ -> root_cause ("DefinitionKind.parse: unknown kind: " ^ kind) j
end

module Declaration = struct
  type t = {
    name : string;
    kind : DeclarationKind.t;
    ty : Type.t;
    init : Expression.t option;
  }

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* name = with_field "name" cast_string o in
    let* ty = with_field "ty" Type.parse o in
    let* kind = DeclarationKind.parse j in
    let* init = with_field "init" (cast_option Expression.parse) o in
    Ok { name; kind; ty; init }

  let to_string (d : t) : string =
    let prefix =
      match d.kind with
      | GlobalVariable d ->
          let space =
            let space = AddressSpace.to_string d.space in
            if space <> "" then "<" ^ space ^ ">" else ""
          in
          let binding =
            d.binding
            |> Option.map (fun x -> ResourceBinding.to_string x ^ " ")
            |> Option.value ~default:""
          in
          binding ^ "var" ^ space
      | Constant -> "const"
      | Override _ -> "override"
    in
    let init =
      d.init
      |> Option.map (fun x -> " = " ^ Expression.to_string x)
      |> Option.value ~default:""
    in
    Printf.sprintf "%s %s: %s%s;" prefix d.name (Type.to_string d.ty) init

  let to_s (d : t) : Indent.t list = [ Line (to_string d) ]

  let map (f : Expression.t -> Expression.t) (d : t) : t =
    { d with init = Option.map f d.init }
end

module ProgramEntry = struct
  type t =
    | EntryPoint of EntryPoint.t
    | Declaration of Declaration.t
    | Function of Function.t

  let parse (j : json) : t j_result =
    let open Rjson in
    let* o = cast_object j in
    let* kind = get_kind o in
    match kind with
    | _ when DeclarationKind.is_kind kind ->
        let* d = Declaration.parse j in
        Ok (Declaration d)
    | "EntryPoint" ->
        let* e = EntryPoint.parse j in
        Ok (EntryPoint e)
    | "Function" ->
        let* f = Function.parse j in
        Ok (Function f)
    | _ -> root_cause ("ProgramEntry.parse: unknown kind: " ^ kind) j

  let map_expression (f : Expression.t -> Expression.t) : t -> t = function
    | EntryPoint e -> EntryPoint (EntryPoint.map (Statement.map f) e)
    | Declaration d -> Declaration (Declaration.map f d)
    | Function e -> Function (Function.map (Statement.map f) e)

  let to_string : t -> string = function
    | EntryPoint e -> EntryPoint.to_string e
    | Declaration d -> Declaration.to_string d
    | Function f -> Function.to_string f

  let to_s : t -> Indent.t list = function
    | EntryPoint e -> EntryPoint.to_s e
    | Declaration e -> Declaration.to_s e
    | Function f -> Function.to_s f
end

module Program = struct
  type t = ProgramEntry.t list

  let map_expression (f : Expression.t -> Expression.t) : t -> t =
    List.map (ProgramEntry.map_expression f)

  let parse (j : json) : t j_result =
    let open Rjson in
    let* l = cast_map ProgramEntry.parse j in
    Ok l

  let to_s : t -> Indent.t list = List.concat_map ProgramEntry.to_s
  let to_string (l : t) : string = Indent.to_string (to_s l)
end
