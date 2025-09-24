let () =
  let major, minor, build, revision = Z3.Version.get_version () in
  Printf.printf "%d.%d.%d\n" major minor build