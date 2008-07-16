open UInt64

module UIMap = Map.Make(UInt64)

class byte b =
object (this)
  val byte = b
  method uint64 = (UInt64.of_int (Byte.to_int byte))
  method uint64_shift n = UInt64.shift_left this#uint64 n
  method uint64_8 = this#uint64_shift 8
  method uint64_16 = this#uint64_shift 16
  method uint64_24 = this#uint64_shift 24
  method uint64_32 = this#uint64_shift 32
  method uint64_40 = this#uint64_shift 40
  method uint64_48 = this#uint64_shift 48
  method uint64_56 = this#uint64_shift 56
  method int = Byte.to_int byte
  method signed =
    if byte > Byte.of_int 127 then
      Int64.of_int (- ((Byte.to_int byte) mod 128))
    else
      Int64.of_int (Byte.to_int byte)
  method string16 =
    let str = "0x  " in
    let str' = Byte.to_string16 byte in
    str.[2] <- str'.[2];
    str.[3] <- str'.[3];
    str
  method string =
    Byte.to_string byte
end

let invert_mask = UInt64.complement

let byte_masks =
  let b0 = of_string "0x00000000000000ff" in
  [|b0;
    shift_left b0 8;
    shift_left b0 16;
    shift_left b0 24;
    shift_left b0 32;
    shift_left b0 40;
    shift_left b0 48;
    shift_left b0 56
  |]

let byte_masks' =
  Array.map invert_mask byte_masks

let wyde_masks =
  let w0 = of_string "0x000000000000ffff" in
  [|w0;
    shift_left w0 16;
    shift_left w0 32;
    shift_left w0 48;
  |]

let wyde_masks' =
  Array.map invert_mask wyde_masks

let tetra_masks =
  let t0 = of_string "0x00000000ffffffff" in
  [|t0;
    shift_left t0 32
  |]

let tetra_masks' =
  Array.map invert_mask tetra_masks

let three = of_int 3
let four = of_int 4
let seven = of_int 7

let one' = complement one
let three' = complement three
let seven' = complement seven

let getbyte m addr =
  try
    (UIMap.find addr m)#uint64
  with Not_found -> zero

let getbyte_string m addr =
  try
    (UIMap.find addr m)#string
  with Not_found -> "0"

let getbyte_string16 m addr =
  try
    (UIMap.find addr m)#string16
  with Not_found -> "0x00"

let max_byte = of_int 0xFF

let setbyte m addr v =
  try
    if v = zero then
      UIMap.remove addr m
    else
      UIMap.add addr (new byte (Byte.of_int (to_int v))) m
  with Invalid_argument "byte" -> invalid_arg "setbyte"

let sign_byte b =
  if b > of_int 127 then
    logor b (complement byte_masks.(0))
  else
    b

let getwyde m addr =
  let addr = UInt64.logand addr one' in
  let b1 = getbyte m addr in
  let b2 = shift_left (getbyte m (succ addr)) 8 in
  logor b1 b2

let max_wyde = of_int 0xFFFF

let setwyde m addr wyde =
  if wyde > max_wyde then
    invalid_arg "setwyde"
  else
    let addr = UInt64.logand addr one' in
    let b1 = logand byte_masks.(0) wyde in
    let b2 = logand byte_masks.(0) (shift_right wyde 8) in
    setbyte (setbyte m addr b1) (succ addr) b2

let sign_wyde w =
  if w > of_int 32767 then
    logor w wyde_masks'.(0)
  else
    w

let gettetra m addr =
  let addr = UInt64.logand addr three' in
  let w1 = getwyde m addr in
  let w2 = getwyde m (add two addr) in
  logor w1 (shift_left w2 16)

let max_tetra = of_int64 0xFFFFFFFFL
let max_signed_tetra = of_int64 2147483647L

let settetra m addr tetra =
  if tetra > max_tetra then
    invalid_arg "settetra"
  else
    let addr = UInt64.logand addr three' in
    let w1 = logand wyde_masks.(0) tetra in
    let w2 = shift_right tetra 16 in
    setwyde (setwyde m addr w1) (add addr two) w2

let sign_tetra t =
  if t > max_signed_tetra then
    logor t tetra_masks'.(0)
  else
    t

let getocta m addr =
  let addr = UInt64.logand addr seven' in
  let t1 = gettetra m addr in
  let t2 = gettetra m (add addr four) in
  logor t1 (shift_left t2 32)

let setocta m addr octa =
  let addr = UInt64.logand addr seven' in
  let t1 = logand tetra_masks.(0) octa in
  let t2 = shift_right octa 32 in
  settetra (settetra m addr t1) (add addr four) t2

let getaux_string16 size m addr =
  let (getfun, len) =
    match size with
      | `Wyde -> getwyde, 4
      | `Tetra -> gettetra, 8
  in
  let bytes = getfun m addr in
  let short = "0x" ^ (String.create len) in
  let long = UInt64.to_string16 bytes in
  let longstart = 16 - len in
  for i = 2 to len+1 do
    short.[i] <- long.[longstart+i];
  done;
  short

let getwyde_string16 m addr = getaux_string16 `Wyde m addr

let gettetra_string16 m addr = getaux_string16 `Tetra m addr

let getocta_string16 m addr = to_string16 (gettetra m addr)

let getwyde_string m addr = to_string (getwyde m addr)

let gettetra_string m addr = to_string (gettetra m addr)

let getocta_string m addr = to_string (getocta m addr)

include UIMap

let compare = UIMap.compare (fun a b -> Byte.compare a#byte b#byte)
