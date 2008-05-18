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
  | Negate of primary
  | Negative of primary
  | Register of num
  | Backward of int
  | Forward of int
  | Char of char
  | Str of string
type term =
    Term of primary
  | Mul of term * primary
  | Div of term * primary
  | FracDiv of term * primary
  | Rem of term * primary
  | SLeft of term * primary
  | SRight of term * primary
type expression =
    Expression of term
  | And of expression * term
  | XOr of expression * term
  | Plus of expression * term
  | Minus of expression * term
type instruction =
    Instruction of string * expression list
  | LInstruction of string * string * expression list
  | L2Instruction of int * string * expression list
  | Empty
