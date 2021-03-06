let special_map =
  let r =
    List.fold_left
      (fun (i,accum) n ->
	 (i+1,(i,"r" ^ n)::accum)
      )
      (0,[])
      ["B";"D";"E";"H";"J";"M";"R";"BB";"C";"N";"O";"S";"I";"T";"TT";"K";"Q";"U";"V";"G";"L";"A";"F";"P";"W";"X";"Y";"Z";"WW";"XX";"YY";"ZZ"]
  in
  snd r
    
module Locations =
struct
  type t = Name of string | NameLoc of string * UInt64.t
  let compare = function
      Name n1 ->
	(function
	     NameLoc (n2,_) -> compare n1 n2
	   | Name n2 -> compare n1 n2
	)
    | NameLoc (n1,_) ->
	(function
	     NameLoc (n2,_) -> compare n1 n2
	   | Name n2 -> compare n1 n2
	)
end

type num = Number of string
	   | Number16 of string
	   | Number8 of string
	   | Number2 of string

type primary = Symbol of string
	       | Constant of num
	       | At
	       | Register of num
	       | Backward of int
	       | Forward of int
	       | Char of char
	       | Str of string

type term = Term of primary
	    | Mul of term * term
	    | Div of term * term
	    | FracDiv of term * term
	    | Rem of term * term
	    | SLeft of term * term
	    | SRight of term * term
	    | And of term * term
	    | XOr of term * term
	    | Plus of term * term
	    | Minus of term * term
	    | Negate of term
	    | Negative of term

type instruction =
  | Instruction of string * term list
  | LInstruction of string * string  * term list
  | L2Instruction of int * string * term list
  | Empty
