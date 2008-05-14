module UIMap :
  sig
    type key = UInt64.t
    type 'a t = 'a Map.Make(UInt64).t
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
class byte :
  int ->
  object
    val byte : char
    method int : int
    method signed : int64
    method string : string
    method string16 : string
    method uint64 : UInt64.uInt64
    method uint64_16 : UInt64.uInt64
    method uint64_24 : UInt64.uInt64
    method uint64_32 : UInt64.uInt64
    method uint64_40 : UInt64.uInt64
    method uint64_48 : UInt64.uInt64
    method uint64_56 : UInt64.uInt64
    method uint64_8 : UInt64.uInt64
    method uint64_shift : int -> UInt64.uInt64
  end
val invert_mask : UInt64.uInt64 -> UInt64.uInt64
val byte_masks : UInt64.uInt64 array
val byte_masks' : UInt64.uInt64 array
val wyde_masks : UInt64.uInt64 array
val wyde_masks' : UInt64.uInt64 array
val tetra_masks : UInt64.uInt64 array
val tetra_masks' : UInt64.uInt64 array
val three : UInt64.uInt64
val four : UInt64.uInt64
val seven : UInt64.uInt64
val one' : UInt64.uInt64
val three' : UInt64.uInt64
val seven' : UInt64.uInt64
val getbyte :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UIMap.key -> UInt64.uInt64
val getbyte_string : < string : string; .. > UIMap.t -> UIMap.key -> string
val getbyte_string16 :
  < string16 : string; .. > UIMap.t -> UIMap.key -> string
val max_byte : UInt64.uInt64
val setbyte : byte UIMap.t -> UIMap.key -> UInt64.uInt64 -> byte UIMap.t
val sign_byte : UInt64.uInt64 -> UInt64.uInt64
val getwyde :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> UInt64.uInt64
val max_wyde : UInt64.uInt64
val setwyde : byte UIMap.t -> UInt64.uInt64 -> UInt64.uInt64 -> byte UIMap.t
val sign_wyde : UInt64.uInt64 -> UInt64.uInt64
val gettetra :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> UInt64.uInt64
val max_tetra : UInt64.uInt64
val max_signed_tetra : UInt64.uInt64
val settetra : byte UIMap.t -> UInt64.uInt64 -> UInt64.uInt64 -> byte UIMap.t
val sign_tetra : UInt64.uInt64 -> UInt64.uInt64
val getocta :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> UInt64.uInt64
val setocta : byte UIMap.t -> UInt64.uInt64 -> UInt64.uInt64 -> byte UIMap.t
val getaux_string16 :
  [< `Tetra | `Wyde ] ->
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
val getwyde_string16 :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
val gettetra_string16 :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
val getocta_string16 :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
val getwyde_string :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
val gettetra_string :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
val getocta_string :
  < uint64 : UInt64.uInt64; .. > UIMap.t -> UInt64.uInt64 -> string
