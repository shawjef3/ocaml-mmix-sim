open Memory
open UInt64
open Machine

let ident x = x

let three = succ two
let four = succ three
let five = succ four
let six = succ five
let seven = succ six
let eight = succ seven
let sixteen = of_int 0x10

(* functions for breaking apart an instruction *)

(* get the instruction number *)

let get_op hex = shift_right (logand byte_masks.(3) hex) 24

(* get any of the three arguments, 64-bit unsigned *)

let get_iu l hex =
  let (mask,shift) =
    match l with
      `X -> (2,16)
    | `Y -> (1,8)
    | `Z -> (0,0)
  in
  shift_right (logand byte_masks.(mask) hex) shift

(* get any of the three arguments, nonnegative int *)

let get_iuint l hex = to_int (get_iu l hex)

(* get any of the three arguments, 64-bit signed *)

let get_i l hex =
  let unsigned = get_iuint l hex in
  let b = (new byte (Byte.of_int unsigned))#signed in
  b

(* get the 64-bit unsigned value of the register named by an argument *)

let get_u l m hex = m.r.(get_iuint l hex)

(* get the 64-bit signed value of a register named by an argument *)

let get_ l m hex = to_int64 (get_u l m hex)

let (get_x, get_y, get_z) = (get_ `X, get_ `Y, get_ `Z)

let (get_xi, get_yi, get_zi) = (get_i `X, get_i `Y, get_i `Z)

let (get_xu, get_yu, get_zu) = (get_u `X, get_u `Y, get_u `Z)

let (get_xiu, get_yiu, get_ziu) = (get_iu `X, get_iu `Y, get_iu `Z)

let (get_xiuint, get_yiuint, get_ziuint) =
  let aux f x = to_int (f x) in
  (
    aux get_xiu,
    aux get_yiu,
    aux get_yiu
  )

let get_yzu = logand wyde_masks.(0)

let get_yz hex =
  let yzu = get_yzu hex in
  to_int64
    (
      if logand (of_int 0x8000) yzu > zero then
	logor (complement wyde_masks.(0)) yzu
      else
	yzu
    )

let get_xyzu = logand (logor wyde_masks.(0) byte_masks.(2))

let get_xyz hex =
  let xyzu = get_xyzu hex in
  let mask = complement (logor byte_masks.(2) wyde_masks.(0)) in
  to_int64
    (
      if logand (of_int32 0x800000l) hex > zero then
	logor mask xyzu
      else
	xyzu
    )

type op = machine -> machine

let incr_m m =
  {m with at = UInt64.add m.at four}

let decr_m m =
  {m with at = UInt64.sub m.at four}

let aux_yz op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let yz = get_yzu hex in
  let m = op m x yz in
  (m : Machine.machine)

let aux_y_z op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let y = get_y m hex in
  let z = get_z m hex in
  let m = op m x y z in
  (m : Machine.machine)

let aux_y_zu op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let y = get_y m hex in
  let z = get_zu m hex in
  let m = op m x y z in
  (m : Machine.machine)

let aux_yint_z op =
  aux_y_z (fun m x y z -> op m x (Int64.to_int y) z)

let aux_y_zint op =
  aux_y_z (fun m x y z -> op m x y (Int64.to_int z))

let aux_yint_zint op =
  aux_y_z (fun m x y z -> op m x (Int64.to_int y) (Int64.to_int z))

let aux_y_zi op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let y = get_y m hex in
  let z = (get_zi hex) in
  let m = op m x y z in
  (m : Machine.machine)

let aux_y_ziu op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let y = get_y m hex in
  let z = get_ziu hex in
  let m = op m x y z in
  (m : Machine.machine)

let aux_yint_zi op =
  aux_y_zi (fun m x y z -> op m x (Int64.to_int y) z)

let aux_y_ziint op =
  aux_y_zi (fun m x y z -> op m x y (Int64.to_int z))

let aux_u_y_z op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let y = get_yu m hex in
  let z = get_zu m hex in
  let m = op m x y z in
  (m : Machine.machine)

let aux_u_yint_zint op =
  aux_u_y_z (fun m x y z -> op m x (to_int y) (to_int z))

let aux_u_y_zi op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let y = get_yu m hex in
  let z = get_ziu hex in
  let m = op m x y z in
  (m : Machine.machine)

let aux_u_yint_zi op =
  aux_u_y_zi (fun m x y z -> op m x (to_int y) z)

let aux_u_yz op m =
  let hex = gettetra m.memory m.at in
  let x = get_xiuint hex in
  let yzu = get_yzu hex in
  let m = op m x yzu in
  (m : Machine.machine)

(*arithmetic*)

let arith_aux_s aux f=
  aux
    (fun m x y z ->
       m.r.(x) <- of_int64 (f y z);
       incr_m m
    )

let arith_aux_u aux f =
  aux
    (fun m x y z ->
       m.r.(x) <- f y z;
       incr_m m
    )

let arith_aux =
  arith_aux_s aux_y_z

let arith_aux_i f =
  aux_y_zi
    (fun m x y z ->
       m.r.(x) <- of_int64 (f y z);
       incr_m m
    )

let arith_aux_u f =
  aux_u_y_z
    (fun m x y z ->
       m.r.(x) <- f y z;
       incr_m m
    )

let arith_aux_ui f =
  aux_u_y_zi
    (fun m x y z ->
       m.r.(x) <- f y z;
       incr_m m
    )

let mul = arith_aux_s aux_y_z Int64.mul
let muli = arith_aux_s aux_y_zi Int64.mul
let mul = arith_aux Int64.mul
let muli = arith_aux_i Int64.mul
let mulu = arith_aux_u UInt64.mul
let mului = arith_aux_ui UInt64.mul
let div = arith_aux Int64.div
let divi = arith_aux_i Int64.div
let divu = arith_aux_u UInt64.div
let divui = arith_aux_ui UInt64.div
let add = arith_aux Int64.add
let addi = arith_aux_i Int64.add
let addu = arith_aux_u UInt64.add
let addui = arith_aux_ui UInt64.add
let sub = arith_aux Int64.sub
let subi = arith_aux_i Int64.sub
let subu = arith_aux_u UInt64.sub
let subui = arith_aux_ui UInt64.sub

(* todo: make sure overflow testing works *)

let neg =
  aux_y_z
    (fun m x y z ->
       m.r.(x) <- of_int64 (Int64.sub y z);
       incr_m m
    )

let negi =
  aux_y_zi
    (fun m x y z ->
       m.r.(x) <- of_int64 (Int64.sub y z);
       incr_m m
    )

let negu = neg

let negui = negi

let addux_aux_aux aux n =
  aux
    (fun m x y z ->
       m.r.(x) <- UInt64.add (UInt64.mul n y) z;
       incr_m m
    )

let addux_aux = addux_aux_aux aux_u_y_z

let adduix_aux = addux_aux_aux aux_u_y_zi

let addu2 = addux_aux two

let addui2 = adduix_aux two

let addu4 = addux_aux four

let addui4 = adduix_aux four

let addu8 = addux_aux eight

let addui8 = adduix_aux eight

let addu16 = addux_aux sixteen

let addui16 = adduix_aux sixteen

(* load from memory *)

let load_aux signfun getfun =
  aux_yint_zint
    (fun m x y z ->
       let loc = UInt64.add m.r.(y) m.r.(z) in
       let n = getfun m.memory loc in
       m.r.(x) <- signfun n;
       incr_m m
    )

let load_auxi signfun getfun =
  aux_yint_zint
    (fun m x y z ->
       let loc = UInt64.add m.r.(y) (of_int z) in
       let n = getfun m.memory loc in
       m.r.(x) <- signfun n;
       incr_m m
    )

let load_auxu = load_aux ident

let load_auxui = load_auxi ident

let ldb = load_aux sign_byte getbyte

let ldbi = load_auxi sign_byte getbyte

let ldbu = load_auxu getbyte

let ldbui = load_auxui getbyte

let ldw = load_aux sign_wyde getwyde

let ldwi = load_auxi sign_wyde getwyde

let ldwu = load_auxu getwyde

let ldwui = load_auxui getwyde

let ldt = load_aux sign_tetra gettetra

let ldti = load_auxi sign_tetra gettetra

let ldtu = load_auxu gettetra

let ldtui = load_auxui gettetra

let ldo = load_auxu getocta

let ldoi = load_auxui getocta

let ldou = ldo

let ldoui = ldoi


(* load high tetrabyte *)

let ldht_aux getz m =
  let hex = get_op m.at in
  let z = getz m hex in
  let x = Int64.to_int (get_xi hex) in
  let y = get_yu m hex in
  let t = gettetra m.memory (UInt64.add y z) in
  m.r.(x) <- shift_left t 32;
  incr_m m

let ldht m =
  ldht_aux get_zu m

let ldhti m =
  ldht_aux (fun _ x -> get_ziu x) m

(* store to memory - todo: handle overflows *)

let set_aux_aux aux setfun mask =
  aux
    (fun m x y z ->
       incr_m
	 {m with
	    memory = setfun m.memory (UInt64.add y z) (logand mask m.r.(x))
	 }
    )

let set_aux = set_aux_aux aux_u_y_z

let set_auxi = set_aux_aux aux_u_y_zi

let stbu = set_aux setbyte byte_masks.(0)

let stbui = set_auxi setbyte byte_masks.(0)

let stwu = set_aux setwyde wyde_masks.(0)

let stwui = set_auxi setwyde wyde_masks.(0)

let sttu = set_aux settetra tetra_masks.(0)

let sttui = set_auxi settetra tetra_masks.(0)

let stou = set_aux setocta UInt64.max

let stoui = set_aux setocta UInt64.max

let sto = stou

let stoi = stoui

let stco =
  aux_u_y_z
    (fun m x y z ->
       incr_m
	 {m with
	    memory = setocta m.memory (UInt64.add y z) (of_int x)
	 }
    )

let stcoi =
  aux_u_y_zi
    (fun m x y z ->
       incr_m
	 {m with
	    memory = setocta m.memory (UInt64.add y z) (of_int x)
	 }
    )

let stht =
  aux_u_y_zi
    (fun m x y z ->
       incr_m
	 {m with
	    memory = setocta m.memory (UInt64.add y z) (shift_right m.r.(x) 32)
	 }
    )

let sthti =
  aux_u_y_zi
    (fun m x y z ->
       incr_m
	 {m with
	    memory = setocta m.memory (UInt64.add y z) (shift_right m.r.(x) 32)
	 }
    )

(* signed versions are the same, except they require overflow checking *)

let stb = stbu

let stbi = stbui

let stw = stwu

let stwi = stwui

let stt = sttu

let stti = sttui

let sto = stou

let stoi = stoui

(* bit magic *)

let bit_aux_aux aux f =
  aux
    (fun m x y z ->
       m.r.(x) <- f y z;
       incr_m m
    )

let bit_aux_aux2 aux f =
  aux
    (fun m x y z ->
       m.r.(x) <- f y (complement z);
       incr_m m
    )

let bit_aux_aux3 aux f =
  aux
    (fun m x y z ->
       m.r.(x) <- complement (f y z);
       incr_m m
    )

let bit_aux = bit_aux_aux aux_u_y_z

let bit_auxi = bit_aux_aux aux_u_y_zi

let bit_aux2 = bit_aux_aux2 aux_u_y_z

let bit_aux2i = bit_aux_aux2 aux_u_y_zi

let bit_aux3 = bit_aux_aux3 aux_u_y_z

let bit_aux3i = bit_aux_aux3 aux_u_y_zi

let and_ = bit_aux UInt64.logand

let andi = bit_auxi UInt64.logand

let or_ = bit_aux UInt64.logor

let ori = bit_auxi UInt64.logor

let xor_ = bit_aux UInt64.logxor

let xori = bit_auxi UInt64.logxor

let andn = bit_aux2 UInt64.logand

let andni = bit_aux2i UInt64.logand

let orn = bit_aux2 UInt64.logor

let orni = bit_aux2 UInt64.logor

let nand = bit_aux3 UInt64.logand

let nandi = bit_aux3i UInt64.logand

let nor = bit_aux3 UInt64.logor

let nori = bit_aux3i UInt64.logor

let nxor = bit_aux3 UInt64.logxor

let nxori = bit_aux3i UInt64.logxor

let mux_aux aux =
  aux
    (fun m x y z ->
       let rM = m.s.(5) in
       m.r.(x) <- logor (logand y rM) (logand z (complement rM));
       incr_m m
    )

let mux = mux_aux aux_u_y_z

let muxi = mux_aux aux_u_y_zi

let dif_aux aux n mask =
  let size = 64 / n in
  aux
    (fun m x y z ->
       let bytes_aux b =
	 Array.mapi
	   (fun i a ->
	      (shift_left (logand b mask.(i)) (i*size))
	   )
	   (Array.create size zero)
       in
       let bytes_y = bytes_aux y in
       let bytes_z = bytes_aux z in
       let diffs = Array.create n zero in
       for i = 0 to n-1 do
	 if bytes_y.(i) >= bytes_z.(i) then
	   diffs.(i) <- UInt64.sub bytes_y.(i) bytes_z.(i);
       done;
       let result = ref zero in
       for i = 0 to n-1 do
	 result := logand !result (shift_left diffs.(n-1-i) (64-((i+1)*size)));
       done;
       m.r.(x) <- !result;
       incr_m m
    )

let bdif = dif_aux aux_u_y_z 8 byte_masks 

let bdifi = dif_aux aux_u_y_zi 8 byte_masks

let wdif = dif_aux aux_u_y_z 16 wyde_masks

let wdifi = dif_aux aux_u_y_zi 16 wyde_masks

let tdif = dif_aux aux_u_y_z 32 tetra_masks

let tdifi = dif_aux aux_u_y_z 32 tetra_masks

let odif_aux aux =
  aux
    (fun m x y z ->
       if z > y then
	 m.r.(x) <- zero
       else
	 m.r.(x) <- UInt64.sub y z;
       incr_m m
    )

let odif = odif_aux aux_u_y_z

let odifi = odif_aux aux_u_y_zi

let sadd_aux aux =
  aux
    (fun m x y z ->
       let temp = logand y (complement z) in
       let rec count total = function
	   0 -> total
	 | n ->
	     count
	       (if logand (shift_left one n) temp > zero then
		  succ total
		else
		  total
	       )
	       (n-1)
       in
       m.r.(x) <- count zero 64;
       incr_m m
    )

let sadd = sadd_aux aux_u_y_z

let saddi = sadd_aux aux_u_y_zi

let wyde_aux f shift =
  aux_u_yz
    (fun m x yz ->
       m.r.(x) <- f m.r.(x) (shift_left yz shift);
       incr_m m
    )

let seth = wyde_aux (fun _ x -> x) 48

let setmh = wyde_aux (fun _ x -> x) 32

let setlh = wyde_aux (fun _ x -> x) 16

let setl = wyde_aux (fun _ x -> x) 0

let orh = wyde_aux logor 48

let ormh = wyde_aux logor 32

let orml = wyde_aux logor 16

let orl = wyde_aux logor 0

let andnh = wyde_aux (fun x y -> UInt64.add x (complement y)) 48

let andnmh = wyde_aux (fun x y -> UInt64.add x (complement y)) 32

let andnml = wyde_aux (fun x y -> UInt64.add x (complement y)) 16

let andnl = wyde_aux (fun x y -> UInt64.add x (complement y)) 0

let slu_aux aux  =
  aux
    (fun m x y z ->
       m.r.(x) <- shift_left y (to_int z);
       incr_m m
    )

let slu = slu_aux aux_u_y_z 

let slui = slu_aux aux_u_y_zi

let sru_aux aux =
  aux
    (fun m x y z ->
       m.r.(x) <- shift_left y (to_int z);
       incr_m m
    )

let sru = sru_aux aux_u_y_z

let srui = sru_aux aux_u_y_zi

(* todo - implement overflow exceptions *)

let sl_aux aux =
  aux
    (fun m x y z ->
       m.r.(x) <- of_int64 (Int64.shift_left y (to_int z));
       m
    )

let sl = sl_aux aux_y_zu

let sli = sl_aux aux_y_ziu

let sr_aux aux =
  aux
    (fun m x y z ->
       m.r.(x) <- of_int64 (Int64.shift_right y (to_int z));
       incr_m m
    )

let sr = sr_aux aux_y_zu
    
let sri = sr_aux aux_y_ziu

(* comparisons *)

let cmp_aux aux =
  aux
    (fun m x y z ->
       m.r.(x) <- of_int (Pervasives.compare y z);
       incr_m m
    )

let cmp = cmp_aux aux_y_z

let cmpi = cmp_aux aux_y_zi

let cmpu = cmp_aux aux_u_y_z

let cmpui = cmp_aux aux_u_y_zi

(* conditionals *)

let cond_aux aux test ffun tfun =
  aux
    (fun m x y z ->
       if test y then
	 ((tfun m x z) : unit)
       else
	 ((ffun m x) : unit);
       incr_m m
    )

let no_change m _ = ()

let set_zero m x =
  m.r.(x) <- zero

let set_z m x z =
  m.r.(x) <- z

let c_aux comparison ffun tfun =
  (cond_aux aux_y_zu comparison ffun tfun,
   cond_aux aux_y_ziu comparison ffun tfun)

let c_sety_aux comparison =
  c_aux comparison no_change set_z

let is_negative y = y < 0L

let (csn,csni) = c_sety_aux is_negative

let is_zero y = y = 0L

let (csz,cszi) = c_sety_aux is_zero

let is_positive y = y > 0L

let (csp,cspi) = c_sety_aux is_positive

let is_odd y = Int64.rem y 2L = 1L

let (csod,csodi) = c_sety_aux is_odd

let is_nonnegative = (fun y -> y >= 0L)

let (csnn,csnni) = c_sety_aux is_nonnegative

let is_nonzero = (fun y -> y <> 0L)

let (csnz,csnzi) = c_sety_aux is_nonzero

let is_nonpositive = (fun y -> y <= 0L)

let (csnp,csnpi) = c_sety_aux is_nonpositive

let is_even y = Int64.rem y 2L = 0L

let (csev,csevi) = c_sety_aux is_even

let c_sety_aux2 comparison =
  c_aux comparison set_zero set_z

let (zsz,zszi) = c_sety_aux2 is_zero

let (zsp,zspi) = c_sety_aux2 is_positive

let (zsod,zsodi) = c_sety_aux2 is_odd

let (zsnn,zsnni) = c_sety_aux2 is_nonnegative

let (zsnz,zsnzi) = c_sety_aux2 is_nonzero

let (zsnp,zsnpi) = c_sety_aux2 is_nonpositive

let (zsev,zsevi) = c_sety_aux2 is_even

(* branches and jumps *)

let faddr at yz = UInt64.add at (UInt64.mul four yz)

let baddr at yz = UInt64.sub at (UInt64.mul four yz)

let b_aux_aux test =
  aux_yz
    (fun m x yz ->
       if test (to_int64 m.r.(x)) then
	 {m with
	    at = faddr m.at yz
	 }
       else
	 m
    )

let bb_aux_aux test =
  aux_yz
    (fun m x yz ->
       if test (to_int64 m.r.(x)) then
	 {m with
	    at = baddr m.at yz
	 }
       else
	 m
    )

let b_aux test =
  (b_aux_aux test,
   bb_aux_aux test
  )

let (bn,bnb) = b_aux is_negative

let (bz,bzb) = b_aux is_zero

let (bp,bpb) = b_aux is_positive

let (bod,bodb) = b_aux is_odd

let (bnn,bnnb) = b_aux is_nonnegative

let (bnz,bnzb) = b_aux is_nonzero

let (bnp,bnpb) = b_aux is_nonpositive

let (bev,bevb) = b_aux is_even

(* probabilistic, for optimization on CPU *)

let (pbn,pbnb,
     pbz,pbzb,
     pbp,pbpb,
     pbod,pbodb,
     pbnn,pbnnb,
     pbnz,pbnzb,
     pbnp,pbnpb,
     pbev,pbevb) =
  (bn,bnb,
   bz,bzb,
   bp,bpb,
   bod,bodb,
   bnn,bnnb,
   bnz,bnzb,
   bnp,bnpb,
   bev,bevb)

let geta =
  aux_yz
    (fun m x yz ->
       m.r.(x) <- faddr m.at yz;
       m
    )

let getab =
  aux_yz
    (fun m x yz ->
       m.r.(x) <- baddr m.at yz;
       m
    )

let baddr24 at xyz = UInt64.add at (UInt64.sub (UInt64.mul four xyz) (of_int 6108864))

let jmp m =
  let xyz = get_xyzu (gettetra m.memory m.at) in
  {m with at = UInt64.add m.at (UInt64.mul four xyz)}

let jmpb m =
  let xyz = get_xyzu (gettetra m.memory m.at) in
  {m with at = baddr24 m.at xyz}

let go_aux aux =
  aux
    (fun m x y z ->
       m.r.(x) <- m.at;
       {m with
	  at = UInt64.add y z
       }
    )

let go = go_aux aux_u_y_z

let goi = go_aux aux_u_y_zi

(* access to special registers *)

(*NOTE: y has to be 0, per spec on p. 34*)
(*Z<=32*)

let get =
  aux_y_zint
    (fun m x y z ->
       m.s.(z) <- m.r.(x);
       incr_m m
    )

let put_aux aux =
  aux
    (fun m x y z ->
       m.s.(x) <- z;
       incr_m m
    )

let put = put_aux aux_u_y_z

let puti = put_aux aux_u_y_zi
