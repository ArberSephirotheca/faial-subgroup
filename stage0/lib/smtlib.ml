type atom =
  | Int of int
  | Bool of bool
  | String of string
  | Symbol of string
  | Keyword of string

type sexp = Atom of atom | List of sexp list
type serializer = { ser_char : char -> unit; ser_string : string -> unit }

let serialize_atom (s : serializer) : atom -> unit = function
  | Int x -> s.ser_string (string_of_int x)
  | Bool x -> s.ser_string (if x then "true" else "false")
  | String x ->
      s.ser_char '"';
      (match String.split_on_char '"' x with
      | [] -> ()
      | x :: l ->
          s.ser_string x;
          List.iter
            (fun fragment ->
              s.ser_char '"';
              s.ser_char '"';
              s.ser_string fragment)
            l);
      s.ser_char '"'
  | Symbol x -> s.ser_string x
  | Keyword x ->
      s.ser_char ':';
      s.ser_string x

let rec serialize_exp (s : serializer) : sexp -> unit = function
  | Atom a -> serialize_atom s a
  | List [] -> s.ser_string "()"
  | List (x :: l) ->
      s.ser_char '(';
      serialize_exp s x;
      List.iter
        (fun x ->
          s.ser_char ' ';
          serialize_exp s x)
        l;
      s.ser_char ')'

let output_atom (out : out_channel) : atom -> unit =
  serialize_atom { ser_string = output_string out; ser_char = output_char out }

let output_sexp (out : out_channel) : sexp -> unit =
  serialize_exp { ser_string = output_string out; ser_char = output_char out }

let atom_to_string (a : atom) : string =
  let b = Buffer.create 100 in
  serialize_atom
    { ser_string = Buffer.add_string b; ser_char = Buffer.add_char b }
    a;
  Buffer.contents b

let sexp_to_string (s : sexp) : string =
  let b = Buffer.create 1024 in
  serialize_exp
    { ser_string = Buffer.add_string b; ser_char = Buffer.add_char b }
    s;
  Buffer.contents b
