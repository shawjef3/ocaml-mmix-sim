open Parsertypes
open Instruction_types
open Instructions
open Instructions_map

type convert =
    Complete of UInt64.t
  | Delayed of ((unit -> string array) * (UInt64.t array -> UInt64.t))

module SMap = Map.Make(String)

let four = UInt64.of_int 4

let incr_addr = UInt64.add four

let ophex s = UInt64.shift_left (hex_of_name s) 24

type pos = One | Two | Three

let spechex h =
  let revmap = List.map (function (a,b) -> (b,a)) special_map in
  List.assoc h revmap

let insert pos h =
  UInt64.shift_left
    h
    (match pos with
	 Three -> 0
       | Two -> 8
       | One -> 16
    )

type arg_temp = WF of UInt64.t | NWF of string * pos

let handle_arg pos = function
    Register r ->
      WF (insert pos (UInt64.of_int r))
  | Special s ->
      WF (insert pos (UInt64.of_int (spechex s)))
  | Location l ->
      NWF (l, pos)
  | Immediate i ->
     WF (insert pos i)

let compose op = function
    WF a -> Complete (UInt64.logor (ophex op) a)
  | NWF (name,pos) ->
      Delayed ((function () -> [|name|]),
	       function a -> UInt64.logor (ophex op) (insert pos a.(0)))

let compose_aux =
    List.fold_left
      UInt64.logor
      UInt64.zero

let rec convert addr map = function
    LInstruction (locname, instr, args) ->
      let map' = SMap.add locname addr map in
      compose addr map' instr args
  | Instruction (instr,args) ->
      compose addr map instr args
  | Is (binder, bindand) ->
      
  | Loc (locer, locand) ->
      

let seven = UInt64.of_int 7
let seven' = UInt64.complement seven
let thirtytwo = UInt64.of_int 32

let compose_convert start instructions =
  List.fold_right
    (fun instr (addr,map,instraccum) ->
       let (newmap,converted) = convert addr map instr in
       (UInt64.add thirtytwo addr, newmap,converted::instraccum)
    )
    instructions
    (UInt64.logand seven' start, SMap.empty, [])

let replace map instrs =
  List.fold_right
    (fun instr accum ->
       match instr with
	   Complete c -> c::accum
	 | Delayed (getfun, returnfun) ->
	     let to_get = getfun () in
	     (returnfun
	       (Array.map
		  (fun key ->
		     SMap.find key map
		  )
		  to_get
	       ))::accum
    )
    instrs
    []

let load_machine_int64 start instructions m =
  let start = UInt64.logand (UInt64.complement seven) start in
  snd
    (List.fold_left
       (fun (addr,accum_m) instr ->
	  let new_m =
	    {accum_m with
	       Machine.memory = Memory.settetra accum_m.Machine.memory addr instr
	    }
	  in
	  (incr_addr addr, new_m)
       )
       (start, m)
       instructions
    )

let load_machine_string start instructions m =
  let instructions =
    List.map
      (fun i ->
	 Parser.instr Lexer.lex (Lexing.from_string i)
      )
      instructions
  in
  let (len,map,instructions) = compose_convert start instructions in
  let instructions = replace map instructions in
  load_machine_int64 start instructions m
