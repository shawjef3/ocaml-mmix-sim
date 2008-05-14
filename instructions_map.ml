open Instructions_dummy
open Instructions

let map = 
  [("TRAP", UInt64.of_string "0x0000000000000000", trap);
   ("FCMP", UInt64.of_string "0x0000000000000001", fcmp);
   ("FUN", UInt64.of_string "0x0000000000000002", fun_);
   ("FEQL", UInt64.of_string "0x0000000000000003", feql);
   ("FADD", UInt64.of_string "0x0000000000000004", fadd);
   ("FIX", UInt64.of_string "0x0000000000000005", fix);
   ("FSUB", UInt64.of_string "0x0000000000000006", fsub);
   ("FIXU", UInt64.of_string "0x0000000000000007", fixu);
   ("FLOT", UInt64.of_string "0x0000000000000008", flot);
   ("FLOTI",UInt64.of_string "0x0000000000000009", floti);
   ("FLOTU", UInt64.of_string "0x000000000000000a", flotu);
   ("FLOTUI",UInt64.of_string "0x000000000000000b", flotui);
   ("SFLOT", UInt64.of_string "0x000000000000000c", sflot);
   ("SFLOTI",UInt64.of_string "0x000000000000000d", sfloti);
   ("SFLOTU", UInt64.of_string "0x000000000000000e", sflotu);
   ("SFLOTUI",UInt64.of_string "0x000000000000000f", sflotui);
   ("FMUL", UInt64.of_string "0x0000000000000010", fmul);
   ("FCMPE", UInt64.of_string "0x0000000000000011", fcmpe);
   ("FUNE", UInt64.of_string "0x0000000000000012", fune);
   ("FEQLE", UInt64.of_string "0x0000000000000013", feqle);
   ("FDIV", UInt64.of_string "0x0000000000000014", fdiv);
   ("FSQRT", UInt64.of_string "0x0000000000000015", fsqrt);
   ("FREM", UInt64.of_string "0x0000000000000016", frem);
   ("FINT", UInt64.of_string "0x0000000000000017", fint);
   ("MUL", UInt64.of_string "0x0000000000000018", mul);
   ("MULI",UInt64.of_string "0x0000000000000019", muli);
   ("MULU", UInt64.of_string "0x000000000000001a", mulu);
   ("MULUI",UInt64.of_string "0x000000000000001b", mului);
   ("DIV", UInt64.of_string "0x000000000000001c", div);
   ("DIVI",UInt64.of_string "0x000000000000001d", divi);
   ("DIVU", UInt64.of_string "0x000000000000001e", divu);
   ("DIVUI",UInt64.of_string "0x000000000000001f", divui);
   ("ADD", UInt64.of_string "0x0000000000000020", add);
   ("ADDI",UInt64.of_string "0x0000000000000021", addi);
   ("ADDU", UInt64.of_string "0x0000000000000022", addu);
   ("ADDUI",UInt64.of_string "0x0000000000000023", addui);
   ("SUB", UInt64.of_string "0x0000000000000024", sub);
   ("SUBI",UInt64.of_string "0x0000000000000025", subi);
   ("SUBU", UInt64.of_string "0x0000000000000026", subu);
   ("SUBUI",UInt64.of_string "0x0000000000000027", subui);
   ("2ADDU", UInt64.of_string "0x0000000000000028", addu2);
   ("2ADDUI",UInt64.of_string "0x0000000000000029", addui2);
   ("4ADDU", UInt64.of_string "0x000000000000002a", addu4);
   ("4ADDUI",UInt64.of_string "0x000000000000002b", addui4);
   ("8ADDU", UInt64.of_string "0x000000000000002c", addu8);
   ("8ADDUI",UInt64.of_string "0x000000000000002d", addui8);
   ("16ADDU", UInt64.of_string "0x000000000000002e", addu16);
   ("16ADDUI",UInt64.of_string "0x000000000000002f", addui16);
   ("CMP", UInt64.of_string "0x0000000000000030", cmp);
   ("CMPI",UInt64.of_string "0x0000000000000031", cmpi);
   ("CMPU", UInt64.of_string "0x0000000000000032", cmpu);
   ("CMPUI",UInt64.of_string "0x0000000000000033", cmpui);
   ("NEG", UInt64.of_string "0x0000000000000034", neg);
   ("NEGI",UInt64.of_string "0x0000000000000035", negi);
   ("NEGU", UInt64.of_string "0x0000000000000036", negu);
   ("NEGUI",UInt64.of_string "0x0000000000000037", negui);
   ("SL", UInt64.of_string "0x0000000000000038", sl);
   ("SLI",UInt64.of_string "0x0000000000000039", sli);
   ("SLU", UInt64.of_string "0x000000000000003a", slu);
   ("SLUI",UInt64.of_string "0x000000000000003b", slui);
   ("SR", UInt64.of_string "0x000000000000003c", sr);
   ("SRI",UInt64.of_string "0x000000000000003d", sri);
   ("SRU", UInt64.of_string "0x000000000000003e", sru);
   ("SRUI",UInt64.of_string "0x000000000000003f", srui);
   ("BN", UInt64.of_string "0x0000000000000040", bn);
   ("BNB",UInt64.of_string "0x0000000000000041", bnb);
   ("BZ", UInt64.of_string "0x0000000000000042", bz);
   ("BZB",UInt64.of_string "0x0000000000000043", bzb);
   ("BP", UInt64.of_string "0x0000000000000044", bp);
   ("BPB",UInt64.of_string "0x0000000000000045", bpb);
   ("BOD", UInt64.of_string "0x0000000000000046", bod);
   ("BODB",UInt64.of_string "0x0000000000000047", bodb);
   ("BNN", UInt64.of_string "0x0000000000000048", bnn);
   ("BNNB",UInt64.of_string "0x0000000000000049", bnnb);
   ("BNZ", UInt64.of_string "0x000000000000004a", bnz);
   ("BNZB",UInt64.of_string "0x000000000000004b", bnzb);
   ("BNP", UInt64.of_string "0x000000000000004c", bnp);
   ("BNPB",UInt64.of_string "0x000000000000004d", bnpb);
   ("BEV", UInt64.of_string "0x000000000000004e", bev);
   ("BEVB",UInt64.of_string "0x000000000000004f", bevb);
   ("PBN", UInt64.of_string "0x0000000000000050", pbn);
   ("PBNB",UInt64.of_string "0x0000000000000051", pbnb);
   ("PBZ", UInt64.of_string "0x0000000000000052", pbz);
   ("PBZB",UInt64.of_string "0x0000000000000053", pbzb);
   ("PBP", UInt64.of_string "0x0000000000000054", pbp);
   ("PBPB",UInt64.of_string "0x0000000000000055", pbpb);
   ("PBOD", UInt64.of_string "0x0000000000000056", pbod);
   ("PBODB",UInt64.of_string "0x0000000000000057", pbodb);
   ("PBNN", UInt64.of_string "0x0000000000000058", pbnn);
   ("PBNNB",UInt64.of_string "0x0000000000000059", pbnnb);
   ("PBNZ", UInt64.of_string "0x000000000000005a", pbnz);
   ("PBNZB",UInt64.of_string "0x000000000000005b", pbnzb);
   ("PBNP", UInt64.of_string "0x000000000000005c", pbnp);
   ("PBNPB",UInt64.of_string "0x000000000000005d", pbnpb);
   ("PBEV", UInt64.of_string "0x000000000000005e", pbev);
   ("PBEVB",UInt64.of_string "0x000000000000005f", pbevb);
   ("CSN", UInt64.of_string "0x0000000000000060", csn);
   ("CSNI",UInt64.of_string "0x0000000000000061", csni);
   ("CSZ", UInt64.of_string "0x0000000000000062", csz);
   ("CSZI",UInt64.of_string "0x0000000000000063", cszi);
   ("CSP", UInt64.of_string "0x0000000000000064", csp);
   ("CSPI",UInt64.of_string "0x0000000000000065", cspi);
   ("CSOD", UInt64.of_string "0x0000000000000066", csod);
   ("CSODI",UInt64.of_string "0x0000000000000067", csodi);
   ("CSNN", UInt64.of_string "0x0000000000000068", csnn);
   ("CSNNI",UInt64.of_string "0x0000000000000069", csnni);
   ("CSNZ", UInt64.of_string "0x000000000000006a", csnz);
   ("CSNZI",UInt64.of_string "0x000000000000006b", csnzi);
   ("CSNP", UInt64.of_string "0x000000000000006c", csnp);
   ("CSNPI",UInt64.of_string "0x000000000000006d", csnpi);
   ("CSEV", UInt64.of_string "0x000000000000006e", csev);
   ("CSEVI",UInt64.of_string "0x000000000000006f", csevi);
   ("ZSN", UInt64.of_string "0x0000000000000070", zsn);
   ("ZSNI",UInt64.of_string "0x0000000000000071", zsni);
   ("ZSZ", UInt64.of_string "0x0000000000000072", zsz);
   ("ZSZI",UInt64.of_string "0x0000000000000073", zszi);
   ("ZSP", UInt64.of_string "0x0000000000000074", zsp);
   ("ZSPI",UInt64.of_string "0x0000000000000075", zspi);
   ("ZSOD", UInt64.of_string "0x0000000000000076", zsod);
   ("ZSODI",UInt64.of_string "0x0000000000000077", zsodi);
   ("ZSNN", UInt64.of_string "0x0000000000000078", zsnn);
   ("ZSNNI",UInt64.of_string "0x0000000000000079", zsnni);
   ("ZSNZ", UInt64.of_string "0x000000000000007a", zsnz);
   ("ZSNZI",UInt64.of_string "0x000000000000007b", zsnzi);
   ("ZSNP", UInt64.of_string "0x000000000000007c", zsnp);
   ("ZSNPI",UInt64.of_string "0x000000000000007d", zsnpi);
   ("ZSEV", UInt64.of_string "0x000000000000007e", zsev);
   ("ZSEVI",UInt64.of_string "0x000000000000007f", zsevi);
   ("LDB", UInt64.of_string "0x0000000000000080", ldb);
   ("LDBI",UInt64.of_string "0x0000000000000081", ldbi);
   ("LDBU", UInt64.of_string "0x0000000000000082", ldbu);
   ("LDBUI",UInt64.of_string "0x0000000000000083", ldbui);
   ("LDW", UInt64.of_string "0x0000000000000084", ldw);
   ("LDWI",UInt64.of_string "0x0000000000000085", ldwi);
   ("LDWU", UInt64.of_string "0x0000000000000086", ldwu);
   ("LDWUI",UInt64.of_string "0x0000000000000087", ldwui);
   ("LDT", UInt64.of_string "0x0000000000000088", ldt);
   ("LDTI",UInt64.of_string "0x0000000000000089", ldti);
   ("LDTU", UInt64.of_string "0x000000000000008a", ldtu);
   ("LDTUI",UInt64.of_string "0x000000000000008b", ldtui);
   ("LDO", UInt64.of_string "0x000000000000008c", ldo);
   ("LDOI",UInt64.of_string "0x000000000000008d", ldoi);
   ("LDOU", UInt64.of_string "0x000000000000008e", ldou);
   ("LDOUI",UInt64.of_string "0x000000000000008f", ldoui);
   ("LDSF", UInt64.of_string "0x0000000000000090", ldsf);
   ("LDSFI",UInt64.of_string "0x0000000000000091", ldsfi);
   ("LDHT", UInt64.of_string "0x0000000000000092", ldht);
   ("LDHTI",UInt64.of_string "0x0000000000000093", ldhti);
   ("CSWAP", UInt64.of_string "0x0000000000000094", cswap);
   ("CSWAPI",UInt64.of_string "0x0000000000000095", cswapi);
   ("LDUNC", UInt64.of_string "0x0000000000000096", ldunc);
   ("LDUNCI",UInt64.of_string "0x0000000000000097", ldunci);
   ("LDVTS", UInt64.of_string "0x0000000000000098", ldvts);
   ("LDVTSI",UInt64.of_string "0x0000000000000099", ldvtsi);
   ("PRELD", UInt64.of_string "0x000000000000009a", preld);
   ("PRELDI",UInt64.of_string "0x000000000000009b", preldi);
   ("PREGO", UInt64.of_string "0x000000000000009c", prego);
   ("PREGOI",UInt64.of_string "0x000000000000009d", pregoi);
   ("GO", UInt64.of_string "0x000000000000009e", go);
   ("GOI",UInt64.of_string "0x000000000000009f", goi);
   ("STB", UInt64.of_string "0x00000000000000a0", stb);
   ("STBI",UInt64.of_string "0x00000000000000a1", stbi);
   ("STBU", UInt64.of_string "0x00000000000000a2", stbu);
   ("STBUI",UInt64.of_string "0x00000000000000a3", stbui);
   ("STW", UInt64.of_string "0x00000000000000a4", stw);
   ("STWI",UInt64.of_string "0x00000000000000a5", stwi);
   ("STWU", UInt64.of_string "0x00000000000000a6", stwu);
   ("STWUI",UInt64.of_string "0x00000000000000a7", stwui);
   ("STT", UInt64.of_string "0x00000000000000a8", stt);
   ("STTI",UInt64.of_string "0x00000000000000a9", stti);
   ("STTU", UInt64.of_string "0x00000000000000aa", sttu);
   ("STTUI",UInt64.of_string "0x00000000000000ab", sttui);
   ("STO", UInt64.of_string "0x00000000000000ac", sto);
   ("STOI",UInt64.of_string "0x00000000000000ad", stoi);
   ("STOU", UInt64.of_string "0x00000000000000ae", stou);
   ("STOUI",UInt64.of_string "0x00000000000000af", stoui);
   ("STSF", UInt64.of_string "0x00000000000000b0", stsf);
   ("STSFI",UInt64.of_string "0x00000000000000b1", stsfi);
   ("STHT", UInt64.of_string "0x00000000000000b2", stht);
   ("STHTI",UInt64.of_string "0x00000000000000b3", sthti);
   ("STCO", UInt64.of_string "0x00000000000000b4", stco);
   ("STCOI",UInt64.of_string "0x00000000000000b5", stcoi);
   ("STUNC", UInt64.of_string "0x00000000000000b6", stunc);
   ("STUNCI",UInt64.of_string "0x00000000000000b7", stunci);
   ("SYNCD", UInt64.of_string "0x00000000000000b8", syncd);
   ("SYNCDI",UInt64.of_string "0x00000000000000b9", syncdi);
   ("PREST", UInt64.of_string "0x00000000000000ba", prest);
   ("PRESTI",UInt64.of_string "0x00000000000000bb", presti);
   ("SYNCID", UInt64.of_string "0x00000000000000bc", syncid);
   ("SYNCIDI",UInt64.of_string "0x00000000000000bd", syncidi);
   ("PUSHGO", UInt64.of_string "0x00000000000000be", pushgo);
   ("PUSHGOI",UInt64.of_string "0x00000000000000bf", pushgoi);
   ("OR", UInt64.of_string "0x00000000000000c0", or_);
   ("ORI",UInt64.of_string "0x00000000000000c1", ori);
   ("ORN", UInt64.of_string "0x00000000000000c2", orn);
   ("ORNI",UInt64.of_string "0x00000000000000c3", orni);
   ("NOR", UInt64.of_string "0x00000000000000c4", nor);
   ("NORI",UInt64.of_string "0x00000000000000c5", nori);
   ("XOR", UInt64.of_string "0x00000000000000c6", xor_);
   ("XORI",UInt64.of_string "0x00000000000000c7", xori);
   ("AND", UInt64.of_string "0x00000000000000c8", and_);
   ("ANDI",UInt64.of_string "0x00000000000000c9", andi);
   ("ANDN", UInt64.of_string "0x00000000000000ca", andn);
   ("ANDNI",UInt64.of_string "0x00000000000000cb", andni);
   ("NAND", UInt64.of_string "0x00000000000000cc", nand);
   ("NANDI",UInt64.of_string "0x00000000000000cd", nandi);
   ("NXOR", UInt64.of_string "0x00000000000000ce", nxor);
   ("NXORI",UInt64.of_string "0x00000000000000cf", nxori);
   ("BDIF", UInt64.of_string "0x00000000000000d0", bdif);
   ("BDIFI",UInt64.of_string "0x00000000000000d1", bdifi);
   ("WDIF", UInt64.of_string "0x00000000000000d2", wdif);
   ("WDIFI",UInt64.of_string "0x00000000000000d3", wdifi);
   ("TDIF", UInt64.of_string "0x00000000000000d4", tdif);
   ("TDIFI",UInt64.of_string "0x00000000000000d5", tdifi);
   ("ODIF", UInt64.of_string "0x00000000000000d6", odif);
   ("ODIFI",UInt64.of_string "0x00000000000000d7", odifi);
   ("MUX", UInt64.of_string "0x00000000000000d8", mux);
   ("MUXI",UInt64.of_string "0x00000000000000d9", muxi);
   ("SADD", UInt64.of_string "0x00000000000000da", sadd);
   ("SADDI",UInt64.of_string "0x00000000000000db", saddi);
   ("MORE", UInt64.of_string "0x00000000000000dc", more);
   ("MOREI",UInt64.of_string "0x00000000000000dd", morei);
   ("MXOR", UInt64.of_string "0x00000000000000de", mxor);
   ("MXORI",UInt64.of_string "0x00000000000000df", mxori);
   ("SETH", UInt64.of_string "0x00000000000000e0", seth);
   ("SETHMH", UInt64.of_string "0x00000000000000e1", sethmh);
   ("SETML", UInt64.of_string "0x00000000000000e2", setml);
   ("SETL", UInt64.of_string "0x00000000000000e3", setl);
   ("INCH", UInt64.of_string "0x00000000000000e4", inch);
   ("INCMH", UInt64.of_string "0x00000000000000e5", incmh);
   ("INCML", UInt64.of_string "0x00000000000000e6", incml);
   ("INCL", UInt64.of_string "0x00000000000000e7", incl);
   ("ORH", UInt64.of_string "0x00000000000000e8", orh);
   ("ORMH", UInt64.of_string "0x00000000000000e9", ormh);
   ("ORML", UInt64.of_string "0x00000000000000ea", orml);
   ("ORL", UInt64.of_string "0x00000000000000eb", orl);
   ("ANDNH", UInt64.of_string "0x00000000000000ec", andnh);
   ("ANDNMH", UInt64.of_string "0x00000000000000ed", andnmh);
   ("ANDNML", UInt64.of_string "0x00000000000000ee", andnml);
   ("ANDNL", UInt64.of_string "0x00000000000000ef", andnl);
   ("JMP", UInt64.of_string "0x00000000000000f0", jmp);
   ("JMPB",UInt64.of_string "0x00000000000000f1", jmpb);
   ("PUSHJ", UInt64.of_string "0x00000000000000f2", pushj);
   ("PUSHJB",UInt64.of_string "0x00000000000000f3", pushjb);
   ("GETA", UInt64.of_string "0x00000000000000f4", geta);
   ("GETAB",UInt64.of_string "0x00000000000000f5", getab);
   ("PUT", UInt64.of_string "0x00000000000000f6", put);
   ("PUTI",UInt64.of_string "0x00000000000000f7", puti);
   ("POP", UInt64.of_string "0x00000000000000f8", pop);
   ("RESUME", UInt64.of_string "0x00000000000000f9", resume);
   ("SAVE", UInt64.of_string "0x00000000000000fa", save);
   ("UNSAVE", UInt64.of_string "0x00000000000000fb", unsave);
   ("SYNC", UInt64.of_string "0x00000000000000fc", sync);
   ("SWYM", UInt64.of_string "0x00000000000000fd", swym);
   ("GET", UInt64.of_string "0x00000000000000fe", get);
   ("TRIP", UInt64.of_string "0x00000000000000ff", trip);
  ]

let fst (x,_,_) = x
let snd (_,y,_) = y
let trd (_,_,z) = z

let instr_of_hex hex =
  trd (List.find (fun (_,ihex,_) -> UInt64.logand hex ihex = hex) map)

let instr_of_name name =
  trd (List.find (fun (iname,_,_) -> iname = name) map)

let name_of_hex hex =
  fst (List.find (fun (_,ihex,_) -> UInt64.logand hex ihex = hex) map)

let hex_of_name name =
  snd (List.find (fun (iname,_,_) -> iname = name || String.lowercase iname = name) map)