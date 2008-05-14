type machine = {
  memory : Memory.byte Memory.UIMap.t;
  r : UInt64.t array;
  s : UInt64.t array;
  at : UInt64.t;
}
val copy : machine -> machine
val create : int -> machine
