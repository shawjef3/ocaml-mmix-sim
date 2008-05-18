open Parsertypes
open Instruction_types
open Instructions
open Instructions_map

type convert =
    Complete of UInt64.t
  | Delayed of ((unit -> string array) * (UInt64.t array -> UInt64.t))

module SMap = Map.Make(String)

module IMap = Map.Make(struct type t = int let compare = compare end)

module Parser_state =
struct
  type ('a,'b) t =
      {
	at : UInt64.t;
	numbered : IMap.t;
	named : SMap.t;
	commands : 'a list;
	errors : 'b list;
      }

  let init =
    {
      at = UInt64.zero;
      numbered = IMap.empty;
      named = SMap.empty;
      commands = [];
      errors = [];
    }      

  let two = UInt64.two

  let four = UInt64.of_int 4

  let eight = UInt64.of_int 8

  let incr_two s = {s with at = UInt64.add two s.at}

  let incr_four s = {s with at = UInt64.add four s.at}

  let incr_eight s = {s with at = UInt64.add eight s.at}

  type label = IL of int | SL of string

  let add_command s ?(label=None) c =
    let s =
      match label with
	  IL i -> {s with numbered = IMap.add i s.at s.numbered}
	| SL s -> {s with named = IMap.add s s.at s.named}
    in
    {s with
       commands = c::s.commands
    }

end

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
		    !accum)

let term x =
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
      Instruction (name,args) -> `I1 (name, List.map term args)
    | LInstruction (label,name,args) -> `I2 (label,name, List.map term args)
    | L2Instruction (locallabel, name, args) ->
	`I3 (locallabel, name, List.map term args)
    | Empty -> `I0

let program instructions =
  List.fold_right
    (fun a accum ->
       match instruction a with
	 `I0 -> accum
       | _ as i -> i::accum
    )
    instructions
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
