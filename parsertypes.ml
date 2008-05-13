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
	       | Negate of primary
	       | Negative of primary
	       | Positive of primary
	       | Register of num
	       | Backward of int
	       | Forward of int
	       | SQuote of char
	       | DQuote of string

type term = Term of primary
	    | Mul of primary * primary
	    | Div of primary * primary
	    | FracDiv of primary * primary
	    | Rem of primary * primary
	    | SLeft of primary * primary
	    | SRight of primary * primary

type expression = Expression of term
		  | And of term * term
		  | XOr of term * term
		  | Plus of term * term
		  | Minus of term * term

type instruction =
  | Instruction of string * expression list
  | LInstruction of string * string  * expression list
  | L2Instruction of int * string * expression list
  | Empty