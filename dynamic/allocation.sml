structure Allocation = struct

  structure LSET = LocationBinarySet
  structure LMAP = LocationBinaryMap

  structure IMAP = IntBinaryMapAux
  structure ISET = IntBinarySetAux
  structure ICT = IntCounter
  structure II = InterInstruction
  structure IM = InterMethod

  open LocationBinaryMap

  datatype clos = datatype InterClosure.closure
  datatype instruction = datatype InterInstruction.code

  datatype location =
    LOC of int |
    (* closid * fldid *)
    FLD of int * int |
    NUL |
    BAS of int

  type locenv = location map

  fun refedLocProg prog = let
    val ls = IMAP.foldli (fn (closid, clos, locs) =>
    (refedLocClos (closid, clos)) @ locs) [] prog in
    LSET.fromList ls end

  and refedLocClos (id, clos) = let
    val ct = ICT.new ()

    fun refedLocMeth meth  = List.foldl (fn (inst, locs) =>
      (refedLocInst inst) @ locs) [] meth

    and refedLocInst inst =
      (List.concat o( List.map refedLoc)) (InterInstruction.getLocs inst)

    and refedLoc (loc as (cloc, _)) = if id = cloc orelse cloc < 0
      then [] else [loc]

    in (case clos of
        TOP (_, _, meth) => refedLocMeth meth
      | FCN (prev, _, _, meth) => refedLocMeth meth) end

  (* generate location map and closure init method *)

  fun genProg prog = let

    val fldlocset = refedLocProg prog
    val fldctmap = IMAP.mapi (fn (closid, _) => ICT.new ()) prog
    fun fldctnext i = (ICT.next o Option.valOf) (IMAP.find (fldctmap, i))
      handle Option => (print "Not Found"; ~1)

    val locmapref = ref (LMAP.empty : (location LMAP.map))
    val initmapref = ref (IMAP.empty : (IM.method IMAP.map))

    fun gened loc = Option.isSome (LMAP.find (! locmapref, loc))

    fun genClos clos = let
      val locct = ICT.new ()

      fun genMeth code = List.map genInst code

      and genInst inst = genLocs (InterInstruction.getLocs inst)

      and genLoc (loc as (closid, l)) = if gened loc then () else let
        val rst = if closid < 0 then
          (case l of (~1) => NUL
                   | _ => BAS l) else
            if l < 0 then (LOC (~ l)) else
            if LSET.member (fldlocset, loc) then
            FLD (closid, fldctnext closid) else
            LOC (ICT.next locct) in
          locmapref := LMAP.insert (! locmapref, loc, rst) end

      and genLocs ls = (List.map genLoc ls; ()) in
        case clos of TOP (_, _, meth)    => (locct := 0; genMeth meth)
                   | FCN (_, _, _, meth) => (locct := 1; genMeth meth) end in

    IMAP.map genClos prog;

    ! locmapref end

  val i2s = Int.toString
  fun l2s (a, b) = "(" ^ (i2s a) ^ "," ^ (i2s b) ^ ")"
  fun lc2s (LOC i) = "LOC " ^ (i2s i)
    | lc2s (FLD (c, i)) = "FLD " ^ (i2s c) ^ " " ^ (i2s i)
    | lc2s NUL = "NUL"
    | lc2s (BAS l) = "BAS " ^ (i2s l)

  fun toString locmap = LMAP.toString locmap

end

structure NewAllocation = struct

  structure LSET = LocationBinarySet
  structure LMAP = LocationBinaryMap

  structure IMAP = IntBinaryMapAux
  structure ISET = IntBinarySetAux
  structure ICT = IntCounter
  structure II = InterInstruction
  structure IM = InterMethod
  structure IP = InterProgram

  datatype clos = datatype InterClosure.closure
  datatype instruction = datatype InterInstruction.code

  datatype location =
    LOC of int |
    (* loc fldid *)
    FLD of int |
    NUL |
    BAS of int

  val valOf  = Option.valOf
  val isSome = Option.isSome

  fun genProg prog = let

    val locmap = ref ((IMAP.map (fn _ => LMAP.empty) prog)
    : (location LMAP.map) IMAP.map)
    val baslm = LMAP.fromListPair ([((~1,~1), NUL)] @
             (List.map (fn (Value.VAL (~1, f)) => ((~1, f), FLD f)
                         | (Value.CON ((~1, f), t)) => ((~1, f), FLD f)
                         | (Value.EXC ((~1, f), t)) => ((~1, f), FLD f))
             InitialSpace.basValueList))
    val _ = locmap := IMAP.insert (! locmap, ~1, baslm)

    val locctmap = IMAP.map (fn TOP _ => ICT.newi 0
                              | FCN _ => ICT.newi 1) prog
    val fldctmap = IMAP.map (fn _ => ICT.newi ~1) prog

    fun locnext i = (ICT.next o valOf) (IMAP.find (locctmap, i))
      handle Option => (TIO.println "locnext"; raise Option)
    fun fldnext i = (ICT.next o valOf) (IMAP.find (fldctmap, i))
      handle Option => (TIO.println "fldnext"; raise Option)

    fun addlm cid (loc as (c, l)) = let
      val lm = ! locmap
      val clm = valOf (IMAP.find (lm, cid))
        handle Option => (TIO.println "addlm-clm"; raise Option)
      val psop = LMAP.find (clm, loc) in
      if isSome psop then () else let
        val p =
        (if c = ~1 then
          (if l = ~1 then NUL else BAS l) else
          (if cid = c then
            (if l <= 0 then
              LOC (~l) else
              LOC (locnext cid)) else
            FLD (fldnext cid)))

        val newclm = LMAP.insert (clm, loc, p)
        val newlm  = IMAP.insert (lm, cid, newclm) in
      locmap := newlm end end

  fun genClos (cid, clos) = let

  fun genMeth meth = let

    val locs = IM.getLocs meth in

    (* genMeth *)
    List.map (fn (c, l) => let
      val path = valOf (IP.path prog cid c)
        handle Option => (TIO.println "genMeth-path"; 
        TIO.println (Int.toString cid);
        TIO.println (Int.toString c);
        raise Option) in
      List.map (fn cid => addlm cid (c, l)) path end) locs end in

    (* genClos *)
    genMeth (case clos of
                  TOP (_, _, m) => m
                | FCN (_, _, _, m) => m) end in
    (* genProg *)
    IMAP.mapi (fn p => genClos p) prog;
    ! locmap end

  val i2s = Int.toString
  fun l2s (a, b) = "(" ^ (i2s a) ^ "," ^ (i2s b) ^ ")"
  fun lc2s (LOC i) = "LOC " ^ (i2s i)
    | lc2s (FLD i) = "FLD " ^ (i2s i)
    | lc2s NUL = "NUL"
    | lc2s (BAS l) = "BAS " ^ (i2s l)

  fun toString m = let
    fun aux locmap = LMAP.toString locmap in
    IMAP.toString m i2s aux " " "\n\n\n" end

end
