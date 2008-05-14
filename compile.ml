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
    Custom "ocamlc -custom -o mmixsim uInt64.o uInt64.cmo memory.cma machine.cma -I +labltk labltk.cma parser.cma lexer.cma parsertypes.cma instructions_dummy.cma instructions.cma instructions_map.cma instruction_types.cma string_to_hex.cma str.cma ui.ml"
  ]

let commands =
  List.fold_right
    (fun a accum ->
       match a with
	   Custom s -> s::accum
	 | MLI s ->
	     ("ocamlc "^s^".mli")::("ocamlc -a -o "^s^".cma "^s^".ml")::accum
	 | MLY s ->
	     ("ocamlyacc "^s^".mly")::("ocamlc "^s^".mli")::("ocamlc -a -o "^s^".cma "^s^".ml")::accum
	 | MLL s ->
	     ("ocamllex "^s^".mll")::("ocamlc -i "^s^".ml > "^s^".mli")::("ocamlc "^s^".mli")::("ocamlc -a -o "^s^".cma "^s^".ml")::accum
    )
    todo
    []

let _ =
  List.iter (fun c -> print_endline c; ignore (Sys.command c)) commands
