val special_map : (int * string) list
module Locations :
  sig
    type t = Name of string | NameLoc of string * UInt64.t
    val compare : t -> t -> int
  end
type num =
    Number of string
  | Number16 of string
  | Number8 of string
  | Number2 of string
type primary =
    Symbol of string
  | Constant of num
  | At
  | Register of num
  | Backward of int
  | Forward of int
  | Char of char
  | Str of string
type term =
    Term of primary
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
    Instruction of string * term list
  | LInstruction of string * string * term list
  | L2Instruction of int * string * term list
  | Empty
