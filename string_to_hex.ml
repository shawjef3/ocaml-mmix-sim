(*
ocaml str.cma uInt64.cma memory.cma machine.cma parser.cma lexer.cma parsertypes.cma instructions_dummy.cma instructions.cma instructions_map.cma instruction_types.cma
*)
open Parsertypes
open Instruction_types
open Instructions
open Instructions_map

type convert =
    Complete of UInt64.t
  | Delayed of ((unit -> string array) * (UInt64.t array -> UInt64.t))

module SMap = Map.Make(String)

module IMap = Map.Make(struct type t = int let compare = compare end)

let ophex s = UInt64.shift_left (hex_of_name s) 24

let pure = function
    Number n -> UInt64.of_string_base n 10
  | Number16 n -> UInt64.of_string_base n 16
  | Number8 n -> UInt64.of_string_base n 8
  | Number2 n -> UInt64.of_string_base n 2

let rec primary = function
    Symbol s -> `Replace s
  | Constant c -> `Immediate c
  | At -> `At
  | Register p -> `Register p
  | Forward f -> `Forward f
  | Backward b -> `Back b
  | Char s -> `Bytes [s]
  | Str d -> `Bytes (let l = String.length d in
		     let accum = ref [] in
		     for i = l-1 downto 0 do
		       accum := d.[i]::!accum
		     done;
		     List.rev !accum)

let rec term x =
  match
    (function
	 Term t -> `Zero (primary t)
       | Mul (x,y) -> `Two (UInt64.mul, (x,y))
       | Div (x,y) -> `Two (UInt64.div, (x,y))
       | FracDiv (x,y) ->
	   `Two ((fun x y ->
		    UInt64.mul
		      (UInt64.div UInt64.max y)
		      x
		 ),
		 (x,y))
       | Rem (x,y) -> `Two (UInt64.rem, (x,y))
       | SLeft (x,y) -> `Two ((fun x y -> UInt64.shift_left x (UInt64.to_int y)), (x,y))
       | SRight (x,y) -> `Two ((fun x y -> UInt64.shift_right x (UInt64.to_int y)), (x,y))
       | And (x,y) -> `Two (UInt64.logand, (x,y))
       | XOr (x,y) -> `Two (UInt64.logxor, (x,y))
       | Plus (x,y) -> `Two (UInt64.add, (x,y))
       | Minus (x,y) -> `Two (UInt64.sub, (x,y))
       | Negate n -> `One (UInt64.complement, n)
       | Negative m -> `One ((UInt64.sub (UInt64.zero)), m)
    )
      x
  with
    `Zero t -> `Zero t
  | `One (op, x) -> `One (op, term x)
  | `Two (op,(x,y)) -> `Two (op, term x, term y)

let instruction =
  function
      (l : int), Instruction (name,args) -> `I1 (l, name, List.map term args)
    | l, LInstruction (label,name,args) -> `I2 (l, label,name, List.map term args)
    | l, L2Instruction (locallabel, name, args) ->
	`I3 (l, locallabel, name, List.map term args)
    | l, Empty -> `I0

let instructions x =
  List.filter ((<>) `I0) (List.map instruction (Parser.program Lexer.lex x))

let arg_types i =
  let aux2 = function
      `Register _ -> true
    | _ -> false
  in
  let rec aux = function
    | `Zero p -> aux2 p
    | `One (_,x) -> aux x
    | `Two (_,x,y) -> aux x || aux y
  in
  let terms =
    match i with
      `I0 -> []
    | `I1 (_,_,terms) -> terms
    | `I2 (_,_,_,terms) -> terms
    | `I3 (_,_,_,terms) -> terms
  in
  List.map (fun t -> if aux t then `Register else `Immediate) terms

let typify i =
  let i = instructions i in
  List.combine i (List.map arg_types i)

let to_binary i =
  Array.fold_right
    (fun (a,typ) accum ->
       match a with
	 `I1 (line, name, args) ->
	   
       | `I2 (line, label, name, args) ->
	   
       | `I3 (line, locallabel, name, args) ->
	   
    )
    i
    {
      accum = Hashtbl.make (Array.length i);
      symbols = SMap.empty;
      mentions = SMap.empty;
      warnings = [];
      errors = [];
      greg = 0;
    }

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
