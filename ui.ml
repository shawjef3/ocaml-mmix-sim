open Labltk
open Tk

let editing errorfun top =
  let f = Frame.create top in
  let t = Text.create ~wrap:`None ~width:40 ~background:`White f in
  let sb = Scrollbar.create ~command:(Text.yview t) f in
  Text.configure ~yscrollcommand:(Scrollbar.set sb) t;
  grid ~column:0 ~row:0 ~sticky:"news" [t];
  grid ~column:1 ~row:0 ~sticky:"ns" [sb];
  (f,t)

let counter top =
  let f = Frame.create ~relief:`Sunken top in
  let i = ref UInt64.zero in
  let l = Label.create ~text:"instruction count: " f in
  let icount = Label.create f in
  let redisplay () = Label.configure ~text:(UInt64.to_string !i) icount in
  redisplay ();
  let incri () =
    i := UInt64.succ !i;
    redisplay ();
  in
  let decri () =
    i := UInt64.pred !i;
    redisplay ();
  in
  let reset () =
    i := UInt64.zero;
    redisplay ();
  in
  grid ~row:0 ~column:0 ~sticky:"ne" [l];
  grid ~row:0 ~column:1 ~sticky:"ne" [icount];
  (f,reset,incri,decri)

let register_view top rnum name =
  let f = Frame.create top in
  let ftop = Frame.create f in
  let fbottom = Frame.create f in
  let l = Label.create ~text:name ftop in
  let sb = Scrollbar.create fbottom in
  let lb1 = Listbox.create
    ~width:18
    ~selectmode:`Browse
    fbottom
  in
  let labellb = Listbox.create
    ~width:3
    ~selectmode:`Browse
    fbottom
  in
  Scrollbar.configure
    ~command:(fun ~scroll -> Listbox.yview lb1 ~scroll;
		Listbox.yview labellb ~scroll)
    sb;
  Listbox.configure
    ~yscrollcommand:
    (fun ~first:f ~last:l ->
       Listbox.yview ~scroll:(`Moveto f) lb1;
     Scrollbar.set ~first:f ~last:l sb;
    )
    labellb;
  Listbox.configure
    ~yscrollcommand:
    (fun ~first:f ~last:l ->
       Listbox.yview ~scroll:(`Moveto f) labellb;
     Scrollbar.set ~first:f ~last:l sb;
    )
    lb1;
  for i = 0 to rnum - 1 do
    Listbox.insert labellb ~index:`End ~texts:([string_of_int i])
  done;
  for i = 0 to rnum - 1 do
    Listbox.insert lb1 ~index:`End ~texts:([UInt64.to_string16 UInt64.zero])
  done;
  let change_reg r v =
    if r > rnum -1 then
      invalid_arg "change_reg"
    else
      (
	let (first,last) = Scrollbar.get sb in
	Listbox.insert lb1 ~index:(`Num r) ~texts:([UInt64.to_string16 v]);
	Listbox.delete lb1 ~first:(`Num (r+1)) ~last:(`Num (r+1));
	Scrollbar.set sb ~first:first ~last:last;
      )
  in
  pack [ftop;fbottom];
  pack ~side:`Right ~fill:`Y ~expand:true [coe sb;coe lb1;coe labellb];
  pack ~side:`Top ~fill:`X ~expand:true [l];
  (f,change_reg)

let menus errorfun top =
  let mbar = Menu.create top in
  let mfile = Menu.create ~tearoff:false mbar in
  let medit = Menu.create ~tearoff:false mbar in
  let mrun = Menu.create ~tearoff:false mbar in
  let mhz = Menu.create ~tearoff:false mbar in
  Menu.add_command ~label:"New" mfile;
  Menu.add_command ~label:"Save" mfile;
  Menu.add_command ~label:"Open" mfile;
  Menu.add_command ~label:"Quit" ~command:closeTk mfile;
  Menu.add_command ~label:"Cut" medit;
  Menu.add_command ~label:"Copy" medit;
  Menu.add_command ~label:"Paste" medit;
  Menu.add_command ~label:"Assemble" mrun;
  Menu.add_command ~label:"Run" mrun;
  List.iter
    (fun hz ->
       Menu.add_command ~label:hz mhz
    )
    ("a lot"::"1/2"::(List.map string_of_int [1;2;4;8;16]));
  Menu.add_cascade ~menu:mfile ~label:"File" mbar;
  Menu.add_cascade ~menu:medit ~label:"Edit" mbar;
  Menu.add_cascade ~menu:mrun ~label:"Run" mbar;
  Menu.add_cascade ~menu:mhz ~label:"Hz" mbar;
  Toplevel.configure ~menu:mbar top;
  ()

let memory_view top height =
  let f = Frame.create top in
  let at = Label.create ~text:("@: " ^ UInt64.to_string16 UInt64.zero) f in
  let set_at addr =
    Label.configure at ~text:("@: " ^ UInt64.to_string16 addr)
  in
  let e = Entry.create ~background:`White f in
  Entry.insert ~index:(`At 0) ~text:"0x0" e;
  let b = Button.create ~text:"Set View" f in
  let address = Listbox.create ~width:18 ~height:height f in
  let lb = Listbox.create ~width:4 ~height:height f in
  let mviewtv = Textvariable.create () in
  Textvariable.set mviewtv "tetrabytes";
  let mview = ref `Tetras in
  let (mb,_) = Optionmenu.create
    ~parent:f
    ~variable:mviewtv
    ["bytes";"wydes";"tetrabytes";"octabytes"]
  in
  let rec mview_handle () =
    Textvariable.handle
      mviewtv
      ~callback:(
	fun () ->
	  mview :=
	    (match Textvariable.get mviewtv with
		 "bytes" -> `Bytes
	       | "wydes" -> `Wydes
	       | "tetrabytes" -> `Tetras
	       | "octabytes" -> `Octas
	       | _ -> failwith "mviewtv"
	    );
	  mview_handle ()
      )
  in
  mview_handle ();
  let set_view m =
    let start =
      try
	UInt64.of_string (Entry.get e)
      with Invalid_argument "UInt64.of_string" ->
	(
	  Entry.delete_range e ~start:(`At 0) ~stop:`End;
	  Entry.insert e ~index:`End ~text:"0";
	  UInt64.zero
	)
    in
    Listbox.delete ~first:(`Num 0) ~last:`End lb;
    Listbox.delete ~first:(`Num 0) ~last:`End address;
    let (getfun, width, step, ignorebits) =
      match !mview with
	  `Bytes -> Memory.getbyte_string16, 4, 1, UInt64.max
	| `Wydes -> Memory.getwyde_string16, 6, 2, Memory.one'
	| `Tetras -> Memory.gettetra_string16, 10, 4, Memory.three'
	| `Octas -> Memory.getocta_string16, 18, 8, Memory.seven'
    in
    let start = UInt64.logand ignorebits start in
    Listbox.configure ~width:width lb;
    for i = 0 to height - 1 do
      let here = UInt64.add start (UInt64.of_int (i*step)) in
      Listbox.insert ~index:`End ~texts:[UInt64.to_string16 here] address;
      Listbox.insert ~index:`End ~texts:[getfun m.Machine.memory here] lb;
    done;
  in
  set_view (Machine.create 1);
  pack ~side:`Right [coe lb;coe address];
  pack ~side:`Bottom [coe b;coe e; coe mb;coe at];
  (f,b,set_view,set_at)

let errors top =
  let f = Frame.create ~relief:`Sunken ~name:"errors" top in
  let tv = Textvariable.create () in
  let l = Label.create ~textvariable:tv f in
  pack [l];
  (f, (fun t -> Textvariable.set tv t))

let middle loadf stepf resetf top =
  let f = Frame.create top in
  let loadb = Button.create ~command:loadf ~text:"Load" f in
  let stepb = Button.create ~command:stepf ~text:"Step" f in
  let resetb = Button.create ~command:resetf ~text:"Reset" f in
  grid ~row:2 ~column:0 [loadb];
  grid ~row:3 ~column:0 [stepb];
  grid ~row:4 ~column:0 [resetb];
  f

let bring_together () =
  let top = opentk () in
  Wm.title_set top "MMIX Simulator";
  Wm.resizable_set ~width:false ~height:false top;
  let m = ref (Machine.create 256) in
  let (errorframe,display_error) = errors top in
  let (e,t) = editing display_error top in
  let (countf,resetcount,incrcount,decrcount) = counter top in
  let (specialframe,changer) = register_view top 256 "main registers" in
  let (registerframe,changes) = register_view top 32 "special registers" in
  let (memframe,membutton,set_view,set_at) = memory_view top 10 in
  menus display_error top;
  let reset_error () = display_error "Ready" in
  reset_error ();
  Button.configure
    ~command:(fun () ->
		try
		  set_view !m;
		  reset_error ();
		with (Invalid_argument "UInt64.of_string") ->
		  display_error "Invalid address"
	     )
    membutton;
  let loadf () =
    let startloc = (`Linechar (0,0),[]) in
    let endloc = (`End,[]) in
    let instr = Text.get t ~start:startloc ~stop:endloc in
    let instrs = Str.split (Str.regexp "\n") instr in
    m := String_to_hex.load_machine_string UInt64.zero instrs !m;
    set_view !m;
  in
  let stepf () =
    let ihex = Memory.gettetra !m.Machine.memory !m.Machine.at in
    let ihex = UInt64.shift_right ihex 24 in
    let f = Instructions_map.instr_of_hex ihex in
    m := f !m;
(*
Incrementing @ is done by the instructions.

    m := {!m with
	    Machine.at = UInt64.add !m.Machine.at (UInt64.of_int 4)
	 };
*)
    set_view !m;
    for i = 0 to 255 do
      changer i !m.Machine.r.(i);
    done;
    for i = 0 to 31 do
      changes i !m.Machine.s.(i)
    done;
    incrcount ();
    set_at (!m.Machine.at);
  in
  let resetf () =
    m := Machine.create 256;
    set_view !m;
    resetcount ();
    for i = 0 to 255 do
      changer i !m.Machine.r.(i);
    done;
    for i = 0 to 31 do
      changes i !m.Machine.s.(i)
    done;
    set_at UInt64.zero;
  in
  let middle = middle loadf stepf resetf top in
  grid ~row:0 ~rowspan:4 ~column:0 [e];
  grid ~row:0 ~column:2 ~rowspan:2 ~sticky:"ns" [registerframe];
  grid ~row:0 ~column:3 ~rowspan:2 [specialframe];
  grid ~row:2 ~column:2 ~columnspan:2 ~rowspan:2 [memframe];
  grid ~row:4 ~column:0 ~columnspan:3 [errorframe];
  grid ~row:4 ~column:3 ~sticky:"e" [countf];
  grid ~row:0 ~column:1 ~sticky:"ns" [middle];
  mainLoop ()

let _ = bring_together ()
