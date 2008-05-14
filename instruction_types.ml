type instr_type = XYZArb | X_Y_Z | X_Y_Zi | X_YZ | XYZ

let type_of_char = function
    '0' -> XYZArb
  | '1' -> X_Y_Z
  | '2' -> X_Y_Zi
  | '3' -> X_YZ
  | '4' -> XYZ


(* copied from http://www-cs-staff.stanford.edu/~knuth/mmop.html *)

let instructions =
"28	2ADDU	times 2 and add unsigned (1)
29	2ADDUI	times 2 and add unsigned immediate (2)
2A	4ADDU	times 4 and add unsigned (1)
2B	4ADDUI	times 4 and add unsigned immediate (2)
2C	8ADDU	times 8 and add unsigned (1)
2D	8ADDUI	times 8 and add unsigned immediate (2)
2E	16ADDU	times 16 and add unsigned (1)
2F	16ADDUI	times 16 and add unsigned immediate (2)
20	ADD	add (1) rA
21	ADDI	add immediate (2) rA
22	ADDU	add unsigned (1)
23	ADDUI	add unsigned immediate (2)
C8	AND	bitwise and (1)
C9	ANDI	bitwise and immediate (2)
CA	ANDN	bitwise and-not (1)>
EC	ANDNH	bitwise and-not high wyde (2)>
CB	ANDNI	bitwise and-not immediate (1)>
EF	ANDNL	bitwise and-not low wyde (3)>
ED	ANDNMH	bitwise and-not medium high wyde (3)>
EE	ANDNML	bitwise and-not medium low wyde (3)>
D0	BDIF	byte difference (1)
D1	BDIFI	byte difference immediate (2)
4E	BEV	branch if even (3)
4F	BEVB	branch if even, backward (3)
40	BN	branch if negative (3)
41	BNB	branch if negative, backward (3)
48	BNN	branch if nonnegative (3)
49	BNNB	branch if nonnegative, backward (3)
4C	BNP	branch if nonpositive (3)
4D	BNPB	branch if nonpositive, backward (3)
4A	BNZ	branch if nonzero (3)
4B	BNZB	branch if nonzero, backward (3)
46	BOD	branch if odd (3)
47	BODB	branch if odd, backward (3)
44	BP	branch if positive (3)
45	BPB	branch if positive, backward (3)
42	BZ	branch if zero (3)
43	BZB	branch if zero, backward (3)
30	CMP	compare (1)
31	CMPI	compare immediate (2)
32	CMPU	compare unsigned (1)
33	CMPUI	compare unsigned immediate (2)
6E	CSEV	conditionally set if even (1)
6F	CSEVI	conditionally set if even immediate (2)
60	CSN	conditionally set if negative (1)
61	CSNI	conditionally set if negative immediate (2)
68	CSNN	conditionally set if nonnegative (2)
69	CSNNI	conditionally set if nonnegative immediate (1)
6C	CSNP	conditionally set if nonpositive (1)
6D	CSNPI	conditionally set if nonpositive immediate (2)
6A	CSNZ	conditionally set if nonzero (1)
6B	CSNZI	conditionally set if nonzero immediate (2)
66	CSOD	conditionally set if odd (1)
67	CSODI	conditionally set if odd immediate (2)
64	CSP	conditionally set if positive (1)
65	CSPI	conditionally set if positive immediate (2)
94	CSWAP	compare and swap octabytes (1) rP
95	CSWAPI	compare and swap octabytes immediate (2) rP
62	CSZ	conditionally set if zero (1)
63	CSZI	conditionally set if zero immediate (2)
1C	DIV	divide (1) rA,rR
1D	DIVI	divide immediate (2) rA,rR
1E	DIVU	divide unsigned (1) rD,rR
1F	DIVUI	divide unsigned immediate rD,rR (2)
04	FADD	floating add (1) rA
01	FCMP	floating compare (1) rA
11	FCMPE	floating compare with respect to epsilon (1) rA,rE
14	FDIV	floating divide (1) rA,rR
03	FEQL	floating equal to (1) rA
13	FEQLE	floating equal with respect to epsilon (1) rA,rE
17	FINT	floating integerize (1, Y=0) rA
05	FIX	convert floating to fixed (1) rA
07	FIXU	convert floating to fixed unsigned (1) rA
08	FLOT	convert fixed to floating (1) rA
09	FLOTI	convert fixed to floating immediate (2) rA
0A	FLOTU	convert fixed to floating unsigned (1) rA
0B	FLOTUI	convert fixed to floating unsigned immediate (2) rA
10	FMUL	floating multiply (1) rA
16	FREM	floating remainder (1) rA
15	FSQRT	floating square root (1, Y=0) rA
06	FSUB	floating subtract (1) rA
02	FUN	floating unordered (1)
12	FUNE	floating unordered with respect to epsilon (1) rE
FE	GET	get from special register (X=register, Y=0, Z=specreg) rA-rZZ
F4	GETA	get address (3)
F5	GETAB	get address backward (3)
9E	GO	go to location (1)
9F	GOI	go to location immediate (2)
E4	INCH	increase by high wyde (3)
E7	INCL	increase by low wyde (3)
E5	INCMH	increase by medium high wyde (3)
E6	INCML	increase by medium low wyde (3)
F0	JMP	jump (4)
F1	JMPB	jump backward (4)
80	LDB	load byte (1)
81	LDBI	load byte immediate (2)
82	LDBU	load byte unsigned (1)
83	LDBUI	load byte unsigned immediate (2)
92	LDHT	load high tetra (1)
93	LDHTI	load high tetra immediate (2)
8C	LDO	load octabyte (1)
8D	LDOI	load octabyte immediate (2)
8E	LDOU	load octabyte unsigned (1)
96	LDUNC	load octabyte uncached (1)
97	LDUNCI	load octabyte uncached immediate (2)
8F	LDOUI	load octabyte unsigned immediate (2)
90	LDSF	load short float (1)
91	LDSFI	load short float immediate (2)
88	LDT	load tetrabyte (1)
89	LDTI	load tetrabyte immediate (2)
8A	LDTU	load tetrabyte unsigned (1)
8B	LDTUI	load tetrabyte unsigned immediate (2)
98	LDVTS	load virtual translation status (1)
99	LDVTSI	load virtual translation status immediate
84	LDW	load wyde (1)
85	LDWI	load wyde immediate (2)
86	LDWU	load wyde unsigned (1)
87	LDWUI	load wyde unsigned immediate (2)
DC	MOR	multiple or (1)
DD	MORI	multiple or immediate (2)
18	MUL	multiply (1) rA
19	MULI	multiply immediate (2) rA
1A	MULU	multiply unsigned (1) rH
1B	MULUI	multiply unsigned immediate (2) rH
D8	MUX	bitwise multiplex (1) rM
D9	MUXI	bitwise multiplex immediate (2) rM
DE	MXOR	multiple exclusive-or (1)
DF	MXORI	multiple exclusive-or immediate (2)
CC	NAND	bitwise not and (1)
CD	NANDI	bitwise not and immediate (2)
34	NEG	negate (1, Y=unsigned immediate) rA
35	NEGI	negate immediate (2, Y=unsigned immediate) rA
36	NEGU	negate unsigned (1, Y=unsigned immediate)
37	NEGUI	negate unsigned immediate (2, Y=unsigned immediate)
C4	NOR	bitwise not-or (1)
C5	NORI	bitwise not-or immediate (2)
CE	NXOR	bitwise not-exclusive-or (1)
CF	NXORI	bitwise not-exclusive-or immediate (2)
D6	ODIF	octa difference (1)
D7	ODIFI	octa difference immediate (2)
C0	OR	bitwise or (1)
E8	ORH	bitwise or with high wyde (3)
C1	ORI	bitwise or immediate (2)
EB	ORL	bitwise or with low wyde (3)
E9	ORMH	bitwise or with medium high wyde (3)
EA	ORML	bitwise or with medium low wyde (3)
C2	ORN	bitwise or-not (1)
C3	ORNI	bitwise or-not immediate (2)
5E	PBEV	probable branch if even (3)
5F	PBEVB	probable branch if even, backward (3)
50	PBN	probable branch if negative (3)
51	PBNB	probable branch if negative, backward (3)
58	PBNN	probable branch if nonnegative (3)
59	PBNNB	probable branch if nonnegative, backward (3)
5C	PBNP	probable branch if nonpositive (3)
5D	PBNPB	probable branch if nonpositive, backward (3)
5A	PBNZ	probable branch if nonzero (3)
5B	PBNZB	probable branch if nonzero, backward (3)
56	PBOD	probable branch if odd (3)
57	PBODB	probable branch if odd, backward (3)
54	PBP	probable branch if positive (3)
55	PBPB	probable branch if positive, backward (3)
52	PBZ	probable branch if zero (3)
53	PBZB	probable branch if zero, backward (3)
F8	POP	pop (3) rJ,rL
9C	PREGO	prefetch to go (1, X=count)
9D	PREGOI	prefetch to go immediate (2, X=count)
9A	PRELD	preload data (1, X=count)
9B	PRELDI	preload data immediate (2, X=count)
BA	PREST	prestore data (1, X=count)
BB	PRESTI	prestore data immediate (2, X=count)
BE	PUSHGO	push registers and go (1) rJ,rL
BF	PUSHGOI	push registers and go immediate (2) rJ,rL
F2	PUSHJ	push registers and jump (3) rJ,rL
F3	PUSHJB	push registers and jump backward (3) rJ,rL
F6	PUT	put into special register (X=specreg, Y=0, Z=register) rA-rZZ
F7	PUTI	put into special register immediate (X=specreg, Y=0, Z=unsigned immediate) rA-rZZ
F9	RESUME	resume after interrupt (4) rW,rX,rY,rZ
DA	SADD	sideways add (1)
DB	SADDI	sideways add immediate (2)
FA	SAVE	save context (X=register, Y=Z=0) rA,rB,rD,rE,rG,rH,rJ,rL,rM,rO,rP,rR,rS,rW,rX,rY,rZ
E0	SETH	set to high wyde (3)
E3	SETL	set to low wyde (3)
E1	SETMH	set to medium high wyde (3)
E2	SETML	set to medium low wyde (3)
0C	SFLOT	convert fixed to short float (1) rA
0D	SFLOTI	convert fixed to short float immediate (2) rA
0E	SFLOTU	convert fixed to short float unsigned (1) rA
0F	SFLOTUI	convert fixed to short float unsigned immediate (2) rA
38	SL	shift left (1) rA
39	SLI	shift left immediate (2) rA
3A	SLU	shift left unsigned (1)
3B	SLUI	shift left unsigned immediate (2)
3C	SR	shift right (1) rA
3D	SRI	Stanford Research Institute (2) rA
3E	SRU	shift right unsigned (1)
3F	SRUI	shift right unsigned immediate (2)
A0	STB	store byte (1) rA
A1	STBI	store byte immediate (2) rA
A2	STBU	store byte unsigned (1)
A3	STBUI	store byte unsigned immediate (2)
B4	STCO	store constant octabyte (X=const, Y=register, Z=register)
B5	STCOI	store constant octabyte immediate (X=const, Y=register, Z=unsigned immediate)
B2	STHT	store high tetra (1)
B3	STHTI	store high tetra immediate (2)
AC	STO	store octabyte (1)
AD	STOI	store octabyte immediate (2)
AE	STOU	store octabyte unsigned (1)
B6	STUNC	store octabyte uncached (1)
B7	STUNCI	store octabyte uncached immediate (2)
AF	STOUI	store octabyte unsigned immediate (2)
B0	STSF	store short float (1) rA
B1	STSFI	store short float immediate (2) rA
A8	STT	store tetrabyte (1) rA
A9	STTI	store tetrabyte immediate (2) rA
AA	STTU	store tetrabyte unsigned (1)
AB	STTUI	store tetrabyte unsigned immediate (2)
A4	STW	store wyde (1) rA
A5	STWI	store wyde immediate (2) rA
A6	STWU	store wyde unsigned (1)
A7	STWUI	store wyde unsigned immediate (2)
24	SUB	subtract (1) rA
25	SUBI	subtract immediate (2) rA
26	SUBU	subtract unsigned (1)
27	SUBUI	subtract unsigned immediate (2)
FD	SWYM	sympathize with your machinery (0)
FC	SYNC	synchronize (4)
B8	SYNCD	synchronize data (1, X=count)
B9	SYNCDI	synchronize data immediate (2, X=count)
BC	SYNCID	synchronize instructions and data (1, X=count)
BD	SYNCIDI	synchronize instructions and data immediate (2, X=count)
D4	TDIF	tetra difference (1)
D5	TDIFI	tetra difference immediate (2)
00	TRAP	trap (0) rBB,rWW,rXX,rYY,rZZ
FF	TRIP	trip (0) rB,rW,rX,rY,rZ
FB	UNSAVE	unsave context (X=Y=0, Z=register) rA,rB,rD,rE,rG,rH,rJ,rL,rM,rO,rP,rR,rS,rW,rX,rY,rZ
D2	WDIF	wyde difference (1)
D3	WDIFI	wyde difference immediate (2)
C6	XOR	bitwise exclusive-or (1)
C7	XORI	bitwise exclusive-or immediate (2)
7E	ZSEV	zero or set if even (1)
7F	ZSEVI	zero or set if even immediate (2)
70	ZSN	zero or set if negative (1)
71	ZSNI	zero or set if negative immediate (2)
78	ZSNN	zero or set if nonnegative (1)
79	ZSNNI	zero or set if nonnegative immediate (2)
7C	ZSNP	zero or set if nonpositive (1)
7D	ZSNPI	zero or set if nonpositive immediate (2)
7A	ZSNZ	zero or set if nonzero (1)
7B	ZSNZI	zero or set if nonzero immediate (2)
76	ZSOD	zero or set if odd (1)
77	ZSODI	zero or set if odd immediate (2)
74	ZSP	zero or set if positive (1)
75	ZSPI	zero or set if positive immediate (2)
72	ZSZ	zero or set if zero (1)
73	ZSZI	zero or set if zero immediate (2)"

let instructions_list =
  Str.split (Str.regexp "\n") instructions

let r s =
  let part1 = "\\(\\([A-F]\\|[0-9]\\)*\\)\\(\t\\| \\)*" in
  let part2 = "\\(\\([A-Z]\\|[0-9]\\)*\\)\\(\t\\| \\)*" in
  let part3 = "[^(]*(\\([0-4]\\)" in
  let r = Str.regexp (part1 ^ part2 ^ part3) in
  if Str.string_match r s 0 then
    (Str.matched_group 1 s, Str.matched_group 4 s, Str.matched_group 7 s)
  else
    let r = Str.regexp (part1 ^ part2) in
    if Str.string_match r s 0 then
      (Str.matched_group 1 s, Str.matched_group 4 s, "1")
    else
      invalid_arg "r"

let instr_types =
  List.map
    (fun s ->
       let (num, instr, type_) = r s in
       (UInt64.of_string_base num 16, instr, type_of_char type_.[0])
    )
    instructions_list

let fst (a,_,_) = a
let snd (_,b,_) = b
let trd (_,_,c) = c

let type_of_int i =
  let i = UInt64.shift_right i 24 in
  let i = UInt64.logand Memory.byte_masks.(0) i in
  trd
    (
      List.find
	(fun (a,b,c) ->
	   UInt64.logand a i = a
	)
	instr_types
    )

