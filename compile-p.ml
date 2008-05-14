type todo =
    Custom of string
  | MLI of string
  | MLL of string
  | MLY of string

let todo =
  [
    Custom "ocaml compile_64.ml";
    MLI "memory";
    MLI "machine";
    MLI "parsertypes";
    MLY "parser";
    MLL "lexer";
    MLI "instructions_dummy";
    MLI "instructions";
    MLI "instructions_map";
    MLI "instruction_types";
    MLI "string_to_hex";
    Custom "ocamlcp -custom -o mmixsim uInt64.c uInt64.ml memory.cma machine.cma -I +labltk labltk.cma parser.cma lexer.cma parsertypes.cma instructions_dummy.cma instructions.cma instructions_map.cma instruction_types.cma string_to_hex.cma str.cma ui.ml profiling.cmo"
  ]

let commands =
  List.fold_right
    (fun a accum ->
       match a with
	   Custom s -> s::accum
	 | MLI s ->
	     ("ocamlcp "^s^".mli")::("ocamlcp -a -o "^s^".cma "^s^".ml")::accum
	 | MLY s ->
	     ("ocamlyacc "^s^".mly")::("ocamlcp "^s^".mli")::("ocamlcp -a -o "^s^".cma "^s^".ml")::accum
	 | MLL s ->
	     ("ocamllex "^s^".mll")::("ocamlcp -i "^s^".ml > "^s^".mli")::("ocamlcp "^s^".mli")::("ocamlcp -a -o "^s^".cma "^s^".ml")::accum
    )
    todo
    []

let _ =
  List.iter (fun c -> print_endline c; ignore (Sys.command c)) commands
