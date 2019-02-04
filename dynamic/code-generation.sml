signature COUNT = sig
  type elem
  val succ : elem -> elem
  val init : elem
end

functor CounterFn (Count : COUNT) = struct
  fun new () = ref Count.init
  fun newi i = ref i
  fun inc ct = ct := (Count.succ (! ct))
  fun next ct = (inc ct; ! ct)
  fun init ct = ct := Count.init
end

structure IntCounter = CounterFn (struct

  type elem = int
  fun succ x = x + 1
  val init = 0

end)

structure Alocation = struct

  structure LocationKey = struct

    type ord_key = (int * int)

    fun compare ((x1,x2), (y1,y2)) = let
      val o1 = Int.compare (x1, y1) in
      if o1 = EQUAL then Int.compare (x2, y2) else o1 end

  end

  structure LocationBinaryMap = OrdMapAuxFn (BinaryMapFn (LocationKey))
  structure LocationBinarySet = LocationBinaryMap.KeySet

  structure LSET = LocationBinarySet
  structure LMAP = LocationBinaryMap

  structure IMAP = IntBinaryMapAux
  structure ISET = IntBinarySetAux
  structure ICT = IntCounter

  open LocationBinaryMap

  datatype clos = datatype InterClosure.closure
  datatype instruction = datatype InterInstruction.code

  datatype location =
    LOC of int |
    (* closid * fldid *)
    FLD of int |
    NUL |
    BAS

  type locenv = location map

  fun refedLocProg prog = let
    val ls = IMAP.foldli (fn (closid, clos, locs) =>
    (refedLocClos (closid, clos)) @ locs) [] prog in
    LSET.fromList ls end

  and refedLocClos (id, clos) = let
    val ct = ICT.new ()

    fun refedLocMeth (meth, _)  = List.foldl (fn (inst, locs) =>
      (refedLocInst inst) @ locs) [] meth

    and refedLocInst inst = 
      (List.concat o( List.map refedLoc)) (InterInstruction.getLocs inst)

    and refedLoc (loc as (cloc, _)) = if id = cloc orelse cloc < 0 
      then [] else [loc]

    in (case clos of
        TOP (_, meth) => refedLocMeth meth
      | FCN (prev, _, meth) => refedLocMeth meth) end


  fun genProg prog = let

    val fldlocset = refedLocProg prog
    val fldctmap = IMAP.mapi (fn (closid, _) => ICT.new ()) prog
    fun fldctnext i = (ICT.next o Option.valOf) (IMAP.find (fldctmap, i))
      handle Option => (print "Not Found"; ~1)
    val locmapref = ref (LMAP.empty : (location LMAP.map))
    fun gened loc = Option.isSome (LMAP.find (! locmapref, loc))

    fun genClos clos = let
      val locct = ICT.new ()

      fun genMeth (code, _) = List.map genInst code

      and genInst inst = genLocs (InterInstruction.getLocs inst)

      and genLoc (loc as (closid, l)) = if gened loc then () else let
        val rst = if closid < 0 then 
          (case l of (~1) => NUL 
                   | _ => BAS) else 
            if l < 0 then (LOC (~ l)) else
            if LSET.member (fldlocset, loc) then
            FLD (fldctnext closid) else
            LOC (ICT.next locct) in
          locmapref := LMAP.insert (! locmapref, loc, rst) end

      and genLocs ls = (List.map genLoc ls; ()) in
        case clos of TOP (_, meth)    => (locct := 0; genMeth meth) 
                   | FCN (_, _, meth) => (locct := 1; genMeth meth) end in
    IMAP.map genClos prog;
    ! locmapref end

  val i2s = Int.toString
  fun l2s (a, b) = "(" ^ (i2s a) ^ "," ^ (i2s b) ^ ")"
  fun lc2s (LOC i) = "LOC " ^ (i2s i)
    | lc2s (FLD i) = "FLD " ^ (i2s i)
    | lc2s NUL = "NUL"
    | lc2s BAS = "BAS"

  fun toString locmap = LMAP.toString locmap l2s lc2s " " "\n"
end

structure CodeGeneration = struct

  structure IMAP = IntBinaryMapAux
  structure II = InterInstruction
  structure ICT = IntCounter

  val isBr = II.isBr
  datatype code = datatype II.code

  fun blockMethod (method, labmax) = let
    val labct = ICT.newi labmax
    val opls2ls = ListAux.fromOptionList

    fun labcode (i :: is) thisLab = (case i of
        LABEL l => (labcode is (SOME l))
      | i       => let
          val thisLab = Option.valOf thisLab 
            handle Option => ICT.next labct
          val (code, brop, rest) = cont (i :: is)
          val seqop = prelab rest in
          (thisLab, code, opls2ls [brop, seqop]) :: (labcode rest seqop) end)
      | labcode [] _ = []

    and prelab [] = NONE
      | prelab (i :: _) = case i of 
       LABEL l => SOME l                                    
           | _ => SOME ((! labct) + 1)

    (* return code, brs, rest*)
    and cont [] = ([], NONE, [])
      | cont ((LABEL _) :: is) = ([], NONE, is)
      | cont (i :: is) = if isBr i then ([i], II.br i, is) else let
        val (block ,brop, rest) = cont is in (i :: block, brop, rest) end in
    labcode method end

  (*fun blockClosure clos = case clos of*)
      (*TOP (_, method)    => TOP (blockMethod method)*)
    (*| FCN (prev, _, method) => FCN (prev, blockMethod method) end*)

  (*fun genProg env prog =  raise Size*)

  (*and genClos env clos =  raise Size*)

  (*and infMethod env cid meth = raise Size*)

  (*and infCode env (MOV    (l1, l2)) = let *)
    (*val p1 = valOf (E.find env l1) *)
    (*handle Option => *)

    (*val p2 = E.find env l2*)

  (*fun infCode env (NEWFCN)*)
    (*| infCode env (NEWSCN)*)
    (*| infCode env (NEWRCD)*)
    (*| infCode env (NEWCON)*)
    (*| infCode env (GETRCD)*)
    (*| infCode env (GETCON)*)
    (*| infCode env (GETINT)*)
    (*| infCode env (CALL  )*)
    (*| infCode env (LABEL )*)
    (*| infCode env (RETURN)*)
end
