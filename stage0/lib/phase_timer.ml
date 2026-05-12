(* Accumulating wall-clock timer keyed by phase name.

   Why keep this around the streaming pipeline:
   each pipeline stage in [App.translate] / [App.run] is a
   [Streamutil.stream] transformer, so naively timing the [|>] line
   measures only thunk construction. [boundary] forces the stream to a
   list (paying the upstream phase's actual work) and records the
   elapsed time against [name]. Inserting one [boundary] between phases
   attributes work to the phase whose elements were just produced —
   prior phases were already materialised by their own boundary, so
   their cost does not bleed into the current measurement.

   Time accumulates across kernels and architectures because the same
   pipeline runs once per (kernel, arch); [add] sums into the entry
   keyed by [name]. [report] returns entries in first-seen order so the
   JSON listing matches the pipeline's natural sequence. *)

let table : (string, float) Hashtbl.t = Hashtbl.create 16
let order : string list ref = ref []

(* When [FAIAL_PHASE_LOG] is set (and not "0"/empty), every completed
   [measure] writes one line to stderr immediately. Survives an
   external timeout that kills the process before [to_json] runs. *)
let log_enabled : bool =
  match Sys.getenv_opt "FAIAL_PHASE_LOG" with
  | None | Some "" | Some "0" -> false
  | _ -> true

let add (name : string) (dt : float) : unit =
  match Hashtbl.find_opt table name with
  | None ->
      Hashtbl.add table name dt;
      order := name :: !order
  | Some t -> Hashtbl.replace table name (t +. dt)

(* Time [f ()] against [name]. [Fun.protect] ensures the elapsed time
   is still recorded if [f] raises (notably [Stop_at_stage], which
   unwinds the per-kernel pipeline in [App.run]).

   [?detail] is a lazy thunk evaluated only when [FAIAL_PHASE_LOG] is
   on; its return value is appended to stderr after the [phase] line.
   Use it to attach call-site diagnostics (e.g. Z3 statistics) without
   paying their formatting cost when logging is off. *)
let measure ?(detail : (unit -> string) option) (name : string) (f : unit -> 'a) : 'a =
  let t0 = Unix.gettimeofday () in
  Fun.protect
    ~finally:(fun () ->
      let dt = Unix.gettimeofday () -. t0 in
      add name dt;
      if log_enabled then begin
        Printf.eprintf "[phase] %s %.3fs\n" name dt;
        (match detail with
         | None -> ()
         | Some d -> Printf.eprintf "%s\n" (d ()));
        flush stderr
      end)
    f

(* Force [s] into a list, time the materialisation against [name], and
   return a fresh re-runnable [from_list] stream. Place [boundary]
   BEFORE the matching [show_or_stop]: when [--show-X] is set, the
   show step iterates the stream — without prior materialisation, the
   subsequent [to_list] in the next boundary would re-run the upstream
   work and double-count. *)
let boundary (name : string) (s : 'a Streamutil.stream) :
    'a Streamutil.stream =
  Streamutil.from_list (measure name (fun () -> Streamutil.to_list s))

let report () : (string * float) list =
  !order |> List.rev |> List.map (fun n -> (n, Hashtbl.find table n))

let to_json () : Yojson.Basic.t =
  `Assoc (report () |> List.map (fun (n, t) -> (n, `Float t)))

let reset () : unit =
  Hashtbl.clear table;
  order := []
