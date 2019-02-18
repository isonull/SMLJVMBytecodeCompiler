structure Allocation = struct

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
        TOP (_, meth) => refedLocMeth meth
      | FCN (prev, _, meth) => refedLocMeth meth) end

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
        case clos of TOP (_, meth)    => (locct := 0; genMeth meth)
                   | FCN (_, _, meth) => (locct := 1; genMeth meth) end in

    IMAP.map genClos prog;

    ! locmapref end

  val i2s = Int.toString
  fun l2s (a, b) = "(" ^ (i2s a) ^ "," ^ (i2s b) ^ ")"
  fun lc2s (LOC i) = "LOC " ^ (i2s i)
    | lc2s (FLD (c, i)) = "FLD " ^ (i2s c) ^ " " ^ (i2s i)
    | lc2s NUL = "NUL"
    | lc2s (BAS l) = "BAS " ^ (i2s l)

  fun toString locmap = LMAP.toString locmap l2s lc2s " " "\n"

end


