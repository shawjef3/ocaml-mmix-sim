let commands =
  [
    "cp mmixsim mmix-sim.app/Contents/MacOS"
  ]

let _ =
  List.iter (fun c -> print_endline c; ignore (Sys.command c)) commands
