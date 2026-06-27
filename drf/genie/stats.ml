(* Re-export of the shared counter table so genie keeps its
   [Drf_genie.Stats] surface while faial-drf records into the same
   module-level state via [Stage0.Stats]. *)
include Stage0.Stats
