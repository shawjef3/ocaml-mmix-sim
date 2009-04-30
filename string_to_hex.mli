type convert =
    Complete of UInt64.t
  | Delayed of ((unit -> string array) * (UInt64.t array -> UInt64.t))
module SMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val incr_addr : UInt64.uInt64 -> UInt64.uInt64
val ophex : string -> UInt64.uInt64
type pos = One | Two | Three
val spechex : string -> int
val insert : pos -> UInt64.uInt64 -> UInt64.uInt64
type arg_temp = WF of UInt64.t | NWF of string * pos
val handle_arg : pos -> Parsertypes.primary -> [> `Replace of string | `Immediate of num | `At | `Register of num | `Forward of int | `Back of int | `Bytes of char list]
val compose1 : string -> arg_temp -> convert
val compose_aux : UInt64.t list -> UInt64.uInt64
val compose2 : string -> arg_temp -> arg_temp -> convert
val compose3 : string -> arg_temp -> arg_temp -> arg_temp -> convert
val convert :
  'a -> 'a SMap.t -> Parsertypes.instruction -> 'a SMap.t * convert
val four : UInt64.uInt64
val seven : UInt64.uInt64
val thirtytwo : UInt64.uInt64
val compose_convert :
  UInt64.uInt64 ->
  Parsertypes.instruction list ->
  UInt64.uInt64 * UInt64.uInt64 SMap.t * convert list
val replace : UInt64.t SMap.t -> convert list -> UInt64.t list
val load_machine_int64 :
  UInt64.uInt64 -> UInt64.uInt64 list -> Machine.machine -> Machine.machine
val load_machine_string :
  UInt64.uInt64 -> string list -> Machine.machine -> Machine.machine
