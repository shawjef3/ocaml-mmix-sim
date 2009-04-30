type machine =
    {
      memory : Memory.byte Memory.UIMap.t;
      r : UInt64.t array;
      s : UInt64.t array;
      at : UInt64.t;
    }

let copy m =
  {
   m with
    r = Array.copy m.r;
    s = Array.copy m.s;
  }

let create rnum =
  {
    memory = Memory.empty;
    r = Array.create rnum UInt64.zero;
    s = Array.create 32 UInt64.zero;
    at = UInt64.zero;
  }


