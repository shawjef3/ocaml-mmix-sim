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
  | Positive of primary
  | Register of num
  | Backward of int
  | Forward of int
  | SQuote of char
  | DQuote of string
type term =
    Term of primary
  | Mul of primary * primary
  | Div of primary * primary
  | FracDiv of primary * primary
  | Rem of primary * primary
  | SLeft of primary * primary
  | SRight of primary * primary
type expression =
    Expression of term
  | And of term * term
  | XOr of term * term
  | Plus of term * term
  | Minus of term * term
type instruction =
    Instruction of string * expression list
  | LInstruction of string * string * expression list
  | L2Instruction of int * string * expression list
  | Empty
