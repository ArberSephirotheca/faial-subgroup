module Range = struct
  type t = { start : int; step : int; stop : int }

  let make ?(start = 0) ?(step = 1) ~stop () : t =
    if step = 0 then failwith "step cannot be zero" else { start; step; stop }

  let to_list (r : t) : int list =
    let cmp : int -> int -> bool = if r.step > 0 then ( <= ) else ( >= ) in
    let rec iter (curr : int) (acc : int list) : int list =
      if cmp r.stop curr then List.rev acc
      else iter (curr + r.step) (curr :: acc)
    in
    iter r.start []
end

let range ?(start = 0) ?(step = 1) (stop : int) : int list =
  Range.make ~start ~step ~stop () |> Range.to_list
