let default_jobs () : int = Domain.recommended_domain_count ()

let jobs_from_env (var : string) : int option =
  match Sys.getenv_opt var with
  | None | Some "" -> None
  | Some s -> ( match int_of_string_opt s with Some n when n > 0 -> Some n | _ -> None)

let test_jobs_var = "FAIAL_TEST_JOBS"

let test_jobs () : int =
  match jobs_from_env test_jobs_var with
  | Some n -> n
  | None -> default_jobs ()

let test_jobs_banner () : string =
  let n = test_jobs () in
  Printf.sprintf "%s=%s, running %d test%s at a time." test_jobs_var
    (Option.value (Sys.getenv_opt test_jobs_var) ~default:"<unset>")
    n
    (if n = 1 then "" else "s")

(* Round-robin striping rather than a work queue: each domain claims
   indices [k], [k + jobs], [k + 2 * jobs] and returns them paired with
   their position, so the merge restores the input order and no state
   is shared. Even striping is only as good as the work is uniform,
   which suits a list of same-cost subprocess calls. *)
let map ~(jobs : int) (f : 'a -> 'b) (l : 'a list) : 'b list =
  let jobs = max 1 jobs in
  if jobs = 1 then List.map f l
  else
    let a = Array.of_list l in
    let n = Array.length a in
    List.init (min jobs n) (fun k ->
        Domain.spawn (fun () ->
            List.init ((n - k + jobs - 1) / jobs) (fun j ->
                let i = k + (j * jobs) in
                (i, f a.(i)))))
    |> List.concat_map Domain.join
    |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
    |> List.map snd
