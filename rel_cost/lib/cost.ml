type t = {
  value: int;
  exact: bool;
  state: Transaction.t option;
}

let value (e:t) : int = e.value

let set_value (value:int) (e:t) : t =
  { e with value }

let make ~state ~value ~exact () : t =
  assert (value >= 0);
  { value; state=Some state; exact }

let from_int ~value ~exact () : t =
  assert (value >= 0);
  { value; state=None; exact }

let (<) (lhs:t) (rhs:t) : bool =
  lhs.value < rhs.value

let (>) (lhs:t) (rhs:t) : bool =
  lhs.value > rhs.value

let (<=) (lhs:t) (rhs:t) : bool =
  lhs.value <= rhs.value

let (>=) (lhs:t) (rhs:t) : bool =
  lhs.value >= rhs.value

let zero : t = from_int ~value:0 ~exact:true ()

let to_string (e:t) : string =
  string_of_int e.value
