val ident : 'a -> 'a
val three : UInt64.uInt64
val four : UInt64.uInt64
val five : UInt64.uInt64
val six : UInt64.uInt64
val seven : UInt64.uInt64
val eight : UInt64.uInt64
val sixteen : UInt64.uInt64
val get_op : UInt64.uInt64 -> UInt64.uInt64
val get_iu : [< `X | `Y | `Z ] -> UInt64.uInt64 -> UInt64.uInt64
val get_iuint : [< `X | `Y | `Z ] -> UInt64.uInt64 -> int
val get_i : [< `X | `Y | `Z ] -> UInt64.uInt64 -> int64
val get_u : [< `X | `Y | `Z ] -> Machine.machine -> UInt64.uInt64 -> UInt64.t
val get_ : [< `X | `Y | `Z ] -> Machine.machine -> UInt64.uInt64 -> int64
val get_x : Machine.machine -> UInt64.uInt64 -> int64
val get_y : Machine.machine -> UInt64.uInt64 -> int64
val get_z : Machine.machine -> UInt64.uInt64 -> int64
val get_xi : UInt64.uInt64 -> int64
val get_yi : UInt64.uInt64 -> int64
val get_zi : UInt64.uInt64 -> int64
val get_xu : Machine.machine -> UInt64.uInt64 -> UInt64.t
val get_yu : Machine.machine -> UInt64.uInt64 -> UInt64.t
val get_zu : Machine.machine -> UInt64.uInt64 -> UInt64.t
val get_xiu : UInt64.uInt64 -> UInt64.uInt64
val get_yiu : UInt64.uInt64 -> UInt64.uInt64
val get_ziu : UInt64.uInt64 -> UInt64.uInt64
val get_xiuint : UInt64.uInt64 -> int
val get_yiuint : UInt64.uInt64 -> int
val get_ziuint : UInt64.uInt64 -> int
val get_yzu : UInt64.uInt64 -> UInt64.uInt64
val get_yz : UInt64.uInt64 -> int64
val get_xyzu : UInt64.uInt64 -> UInt64.uInt64
val get_xyz : UInt64.uInt64 -> int64
type op = Machine.machine -> Machine.machine
val incr_m : Machine.machine -> Machine.machine
val decr_m : Machine.machine -> Machine.machine
val aux_yz :
  (Machine.machine -> int -> UInt64.uInt64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_y_z :
  (Machine.machine -> int -> int64 -> int64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_y_zu :
  (Machine.machine -> int -> int64 -> UInt64.t -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_yint_z :
  (Machine.machine -> int -> int -> int64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_y_zint :
  (Machine.machine -> int -> int64 -> int -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_yint_zint :
  (Machine.machine -> int -> int -> int -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_y_zi :
  (Machine.machine -> int -> int64 -> int64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_y_ziu :
  (Machine.machine -> int -> int64 -> UInt64.uInt64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_yint_zi :
  (Machine.machine -> int -> int -> int64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_y_ziint :
  (Machine.machine -> int -> int64 -> int -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_u_y_z :
  (Machine.machine -> int -> UInt64.t -> UInt64.t -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_u_yint_zint :
  (Machine.machine -> int -> int -> int -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_u_y_zi :
  (Machine.machine -> int -> UInt64.t -> UInt64.uInt64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_u_yint_zi :
  (Machine.machine -> int -> int -> UInt64.uInt64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val aux_u_yz :
  (Machine.machine -> int -> UInt64.uInt64 -> Machine.machine) ->
  Machine.machine -> Machine.machine
val arith_aux_s :
  ((Machine.machine -> int -> 'a -> 'b -> Machine.machine) -> 'c) ->
  ('a -> 'b -> int64) -> 'c
val arith_aux :
  (int64 -> int64 -> int64) -> Machine.machine -> Machine.machine
val arith_aux_i :
  (int64 -> int64 -> int64) -> Machine.machine -> Machine.machine
val arith_aux_u :
  (UInt64.t -> UInt64.t -> UInt64.t) -> Machine.machine -> Machine.machine
val arith_aux_ui :
  (UInt64.t -> UInt64.uInt64 -> UInt64.t) ->
  Machine.machine -> Machine.machine
val mul : Machine.machine -> Machine.machine
val muli : Machine.machine -> Machine.machine
val mulu : Machine.machine -> Machine.machine
val mului : Machine.machine -> Machine.machine
val div : Machine.machine -> Machine.machine
val divi : Machine.machine -> Machine.machine
val divu : Machine.machine -> Machine.machine
val divui : Machine.machine -> Machine.machine
val add : Machine.machine -> Machine.machine
val addi : Machine.machine -> Machine.machine
val addu : Machine.machine -> Machine.machine
val addui : Machine.machine -> Machine.machine
val sub : Machine.machine -> Machine.machine
val subi : Machine.machine -> Machine.machine
val subu : Machine.machine -> Machine.machine
val subui : Machine.machine -> Machine.machine
val neg : Machine.machine -> Machine.machine
val negi : Machine.machine -> Machine.machine
val negu : Machine.machine -> Machine.machine
val negui : Machine.machine -> Machine.machine
val addux_aux_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  UInt64.uInt64 -> 'a
val addux_aux : UInt64.uInt64 -> Machine.machine -> Machine.machine
val adduix_aux : UInt64.uInt64 -> Machine.machine -> Machine.machine
val addu2 : Machine.machine -> Machine.machine
val addui2 : Machine.machine -> Machine.machine
val addu4 : Machine.machine -> Machine.machine
val addui4 : Machine.machine -> Machine.machine
val addu8 : Machine.machine -> Machine.machine
val addui8 : Machine.machine -> Machine.machine
val addu16 : Machine.machine -> Machine.machine
val addui16 : Machine.machine -> Machine.machine
val load_aux :
  ('a -> UInt64.t) ->
  (Memory.byte Memory.UIMap.t -> UInt64.uInt64 -> 'a) ->
  Machine.machine -> Machine.machine
val load_auxi :
  ('a -> UInt64.t) ->
  (Memory.byte Memory.UIMap.t -> UInt64.uInt64 -> 'a) ->
  Machine.machine -> Machine.machine
val load_auxu :
  (Memory.byte Memory.UIMap.t -> Memory.UIMap.key -> UInt64.t) ->
  Machine.machine -> Machine.machine
val load_auxui :
  (Memory.byte Memory.UIMap.t -> Memory.UIMap.key -> UInt64.t) ->
  Machine.machine -> Machine.machine
val ldb : Machine.machine -> Machine.machine
val ldbi : Machine.machine -> Machine.machine
val ldbu : Machine.machine -> Machine.machine
val ldbui : Machine.machine -> Machine.machine
val ldw : Machine.machine -> Machine.machine
val ldwi : Machine.machine -> Machine.machine
val ldwu : Machine.machine -> Machine.machine
val ldwui : Machine.machine -> Machine.machine
val ldt : Machine.machine -> Machine.machine
val ldti : Machine.machine -> Machine.machine
val ldtu : Machine.machine -> Machine.machine
val ldtui : Machine.machine -> Machine.machine
val ldo : Machine.machine -> Machine.machine
val ldoi : Machine.machine -> Machine.machine
val ldou : Machine.machine -> Machine.machine
val ldoui : Machine.machine -> Machine.machine
val ldht_aux :
  (Machine.machine -> UInt64.uInt64 -> UInt64.uInt64) ->
  Machine.machine -> Machine.machine
val ldht : Machine.machine -> Machine.machine
val ldhti : Machine.machine -> Machine.machine
val set_aux_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  (Memory.byte Memory.UIMap.t ->
   UInt64.uInt64 -> UInt64.uInt64 -> Memory.byte Memory.UIMap.t) ->
  UInt64.uInt64 -> 'a
val set_aux :
  (Memory.byte Memory.UIMap.t ->
   Memory.UIMap.key -> UInt64.uInt64 -> Memory.byte Memory.UIMap.t) ->
  UInt64.uInt64 -> Machine.machine -> Machine.machine
val set_auxi :
  (Memory.byte Memory.UIMap.t ->
   Memory.UIMap.key -> UInt64.uInt64 -> Memory.byte Memory.UIMap.t) ->
  UInt64.uInt64 -> Machine.machine -> Machine.machine
val stbu : Machine.machine -> Machine.machine
val stbui : Machine.machine -> Machine.machine
val stwu : Machine.machine -> Machine.machine
val stwui : Machine.machine -> Machine.machine
val sttu : Machine.machine -> Machine.machine
val sttui : Machine.machine -> Machine.machine
val stou : Machine.machine -> Machine.machine
val stoui : Machine.machine -> Machine.machine
val stco : Machine.machine -> Machine.machine
val stcoi : Machine.machine -> Machine.machine
val stht : Machine.machine -> Machine.machine
val sthti : Machine.machine -> Machine.machine
val stb : Machine.machine -> Machine.machine
val stbi : Machine.machine -> Machine.machine
val stw : Machine.machine -> Machine.machine
val stwi : Machine.machine -> Machine.machine
val stt : Machine.machine -> Machine.machine
val stti : Machine.machine -> Machine.machine
val sto : Machine.machine -> Machine.machine
val stoi : Machine.machine -> Machine.machine
val bit_aux_aux :
  ((Machine.machine -> int -> 'a -> 'b -> Machine.machine) -> 'c) ->
  ('a -> 'b -> UInt64.t) -> 'c
val bit_aux_aux2 :
  ((Machine.machine -> int -> 'a -> UInt64.uInt64 -> Machine.machine) -> 'b) ->
  ('a -> UInt64.uInt64 -> UInt64.t) -> 'b
val bit_aux_aux3 :
  ((Machine.machine -> int -> 'a -> 'b -> Machine.machine) -> 'c) ->
  ('a -> 'b -> UInt64.uInt64) -> 'c
val bit_aux :
  (UInt64.t -> UInt64.t -> UInt64.t) -> Machine.machine -> Machine.machine
val bit_auxi :
  (UInt64.t -> UInt64.uInt64 -> UInt64.t) ->
  Machine.machine -> Machine.machine
val bit_aux2 :
  (UInt64.t -> UInt64.uInt64 -> UInt64.t) ->
  Machine.machine -> Machine.machine
val bit_aux2i :
  (UInt64.t -> UInt64.uInt64 -> UInt64.t) ->
  Machine.machine -> Machine.machine
val bit_aux3 :
  (UInt64.t -> UInt64.t -> UInt64.uInt64) ->
  Machine.machine -> Machine.machine
val bit_aux3i :
  (UInt64.t -> UInt64.uInt64 -> UInt64.uInt64) ->
  Machine.machine -> Machine.machine
val and_ : Machine.machine -> Machine.machine
val andi : Machine.machine -> Machine.machine
val or_ : Machine.machine -> Machine.machine
val ori : Machine.machine -> Machine.machine
val xor_ : Machine.machine -> Machine.machine
val xori : Machine.machine -> Machine.machine
val andn : Machine.machine -> Machine.machine
val andni : Machine.machine -> Machine.machine
val orn : Machine.machine -> Machine.machine
val orni : Machine.machine -> Machine.machine
val nand : Machine.machine -> Machine.machine
val nandi : Machine.machine -> Machine.machine
val nor : Machine.machine -> Machine.machine
val nori : Machine.machine -> Machine.machine
val nxor : Machine.machine -> Machine.machine
val nxori : Machine.machine -> Machine.machine
val mux_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val mux : Machine.machine -> Machine.machine
val muxi : Machine.machine -> Machine.machine
val dif_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  int -> UInt64.uInt64 array -> 'a
val bdif : Machine.machine -> Machine.machine
val bdifi : Machine.machine -> Machine.machine
val wdif : Machine.machine -> Machine.machine
val wdifi : Machine.machine -> Machine.machine
val tdif : Machine.machine -> Machine.machine
val tdifi : Machine.machine -> Machine.machine
val odif_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val odif : Machine.machine -> Machine.machine
val odifi : Machine.machine -> Machine.machine
val sadd_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val sadd : Machine.machine -> Machine.machine
val saddi : Machine.machine -> Machine.machine
val wyde_aux :
  (UInt64.t -> UInt64.uInt64 -> UInt64.t) ->
  int -> Machine.machine -> Machine.machine
val seth : Machine.machine -> Machine.machine
val setmh : Machine.machine -> Machine.machine
val setlh : Machine.machine -> Machine.machine
val setl : Machine.machine -> Machine.machine
val orh : Machine.machine -> Machine.machine
val ormh : Machine.machine -> Machine.machine
val orml : Machine.machine -> Machine.machine
val orl : Machine.machine -> Machine.machine
val andnh : Machine.machine -> Machine.machine
val andnmh : Machine.machine -> Machine.machine
val andnml : Machine.machine -> Machine.machine
val andnl : Machine.machine -> Machine.machine
val slu_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val slu : Machine.machine -> Machine.machine
val slui : Machine.machine -> Machine.machine
val sru_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val sru : Machine.machine -> Machine.machine
val srui : Machine.machine -> Machine.machine
val sl_aux :
  ((Machine.machine -> int -> int64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val sl : Machine.machine -> Machine.machine
val sli : Machine.machine -> Machine.machine
val sr_aux :
  ((Machine.machine -> int -> int64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val sr : Machine.machine -> Machine.machine
val sri : Machine.machine -> Machine.machine
val cmp_aux :
  ((Machine.machine -> int -> 'a -> 'a -> Machine.machine) -> 'b) -> 'b
val cmp : Machine.machine -> Machine.machine
val cmpi : Machine.machine -> Machine.machine
val cmpu : Machine.machine -> Machine.machine
val cmpui : Machine.machine -> Machine.machine
val cond_aux :
  ((Machine.machine -> 'a -> 'b -> 'c -> Machine.machine) -> 'd) ->
  ('b -> bool) ->
  (Machine.machine -> 'a -> unit) ->
  (Machine.machine -> 'a -> 'c -> unit) -> 'd
val no_change : 'a -> 'b -> unit
val set_zero : Machine.machine -> int -> unit
val set_z : Machine.machine -> int -> UInt64.t -> unit
val c_aux :
  (int64 -> bool) ->
  (Machine.machine -> int -> unit) ->
  (Machine.machine -> int -> UInt64.t -> unit) ->
  (Machine.machine -> Machine.machine) * (Machine.machine -> Machine.machine)
val c_sety_aux :
  (int64 -> bool) ->
  (Machine.machine -> Machine.machine) * (Machine.machine -> Machine.machine)
val is_negative : int64 -> bool
val csn : Machine.machine -> Machine.machine
val csni : Machine.machine -> Machine.machine
val is_zero : int64 -> bool
val csz : Machine.machine -> Machine.machine
val cszi : Machine.machine -> Machine.machine
val is_positive : int64 -> bool
val csp : Machine.machine -> Machine.machine
val cspi : Machine.machine -> Machine.machine
val is_odd : int64 -> bool
val csod : Machine.machine -> Machine.machine
val csodi : Machine.machine -> Machine.machine
val is_nonnegative : int64 -> bool
val csnn : Machine.machine -> Machine.machine
val csnni : Machine.machine -> Machine.machine
val is_nonzero : int64 -> bool
val csnz : Machine.machine -> Machine.machine
val csnzi : Machine.machine -> Machine.machine
val is_nonpositive : int64 -> bool
val csnp : Machine.machine -> Machine.machine
val csnpi : Machine.machine -> Machine.machine
val is_even : int64 -> bool
val csev : Machine.machine -> Machine.machine
val csevi : Machine.machine -> Machine.machine
val c_sety_aux2 :
  (int64 -> bool) ->
  (Machine.machine -> Machine.machine) * (Machine.machine -> Machine.machine)
val zsz : Machine.machine -> Machine.machine
val zszi : Machine.machine -> Machine.machine
val zsp : Machine.machine -> Machine.machine
val zspi : Machine.machine -> Machine.machine
val zsod : Machine.machine -> Machine.machine
val zsodi : Machine.machine -> Machine.machine
val zsnn : Machine.machine -> Machine.machine
val zsnni : Machine.machine -> Machine.machine
val zsnz : Machine.machine -> Machine.machine
val zsnzi : Machine.machine -> Machine.machine
val zsnp : Machine.machine -> Machine.machine
val zsnpi : Machine.machine -> Machine.machine
val zsev : Machine.machine -> Machine.machine
val zsevi : Machine.machine -> Machine.machine
val faddr : UInt64.uInt64 -> UInt64.uInt64 -> UInt64.uInt64
val baddr : UInt64.uInt64 -> UInt64.uInt64 -> UInt64.uInt64
val b_aux_aux : (int64 -> bool) -> Machine.machine -> Machine.machine
val bb_aux_aux : (int64 -> bool) -> Machine.machine -> Machine.machine
val b_aux :
  (int64 -> bool) ->
  (Machine.machine -> Machine.machine) * (Machine.machine -> Machine.machine)
val bn : Machine.machine -> Machine.machine
val bnb : Machine.machine -> Machine.machine
val bz : Machine.machine -> Machine.machine
val bzb : Machine.machine -> Machine.machine
val bp : Machine.machine -> Machine.machine
val bpb : Machine.machine -> Machine.machine
val bod : Machine.machine -> Machine.machine
val bodb : Machine.machine -> Machine.machine
val bnn : Machine.machine -> Machine.machine
val bnnb : Machine.machine -> Machine.machine
val bnz : Machine.machine -> Machine.machine
val bnzb : Machine.machine -> Machine.machine
val bnp : Machine.machine -> Machine.machine
val bnpb : Machine.machine -> Machine.machine
val bev : Machine.machine -> Machine.machine
val bevb : Machine.machine -> Machine.machine
val pbn : Machine.machine -> Machine.machine
val pbnb : Machine.machine -> Machine.machine
val pbz : Machine.machine -> Machine.machine
val pbzb : Machine.machine -> Machine.machine
val pbp : Machine.machine -> Machine.machine
val pbpb : Machine.machine -> Machine.machine
val pbod : Machine.machine -> Machine.machine
val pbodb : Machine.machine -> Machine.machine
val pbnn : Machine.machine -> Machine.machine
val pbnnb : Machine.machine -> Machine.machine
val pbnz : Machine.machine -> Machine.machine
val pbnzb : Machine.machine -> Machine.machine
val pbnp : Machine.machine -> Machine.machine
val pbnpb : Machine.machine -> Machine.machine
val pbev : Machine.machine -> Machine.machine
val pbevb : Machine.machine -> Machine.machine
val geta : Machine.machine -> Machine.machine
val getab : Machine.machine -> Machine.machine
val baddr24 : UInt64.uInt64 -> UInt64.uInt64 -> UInt64.uInt64
val jmp : Machine.machine -> Machine.machine
val jmpb : Machine.machine -> Machine.machine
val go_aux :
  ((Machine.machine ->
    int -> UInt64.uInt64 -> UInt64.uInt64 -> Machine.machine) ->
   'a) ->
  'a
val go : Machine.machine -> Machine.machine
val goi : Machine.machine -> Machine.machine
val get : Machine.machine -> Machine.machine
val put_aux :
  ((Machine.machine -> int -> 'a -> UInt64.t -> Machine.machine) -> 'b) -> 'b
val put : Machine.machine -> Machine.machine
val puti : Machine.machine -> Machine.machine
