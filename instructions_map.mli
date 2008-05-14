val map :
  (string * UInt64.uInt64 * (Machine.machine -> Machine.machine)) list
val fst : 'a * 'b * 'c -> 'a
val snd : 'a * 'b * 'c -> 'b
val trd : 'a * 'b * 'c -> 'c
val instr_of_hex : UInt64.uInt64 -> Machine.machine -> Machine.machine
val instr_of_name : string -> Machine.machine -> Machine.machine
val name_of_hex : UInt64.uInt64 -> string
val hex_of_name : string -> UInt64.uInt64
