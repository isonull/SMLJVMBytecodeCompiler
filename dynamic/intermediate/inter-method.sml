structure InterMethod = struct 

  structure II = InterInstruction
  structure ISET = IntBinarySetAux
  structure IM = IntBinaryMapAux
  structure IS = IntBinarySetAux

  datatype code = datatype II.code

  type labmax = int
  type method = II.instruction list
  type block  = (int option) * (int list) * method

  val valOf = Option.valOf
  val isSome = Option.isSome


  val getLocs = ListAux.rmDup o
      (List.concat o (List.map (fn i => II.getLocs i))) 

  val getRetlocs = ListAux.rmDup o
      (List.concat o (List.map (fn i => II.getRetlocs i))) 

  fun replaceLocs map method = List.map (II.replaceLocs map) method

  fun getRefs m = List.foldl (fn (i, r) => (II.getRefs i) @ r) [] m

  fun getLabelIndex m l = ListAux.findIndex m (LABEL l)

  fun getBlockMap  meth = let

    fun genBlocks meth = let
      fun bl (i :: is) b bs l = 
        (case i of
            LABEL m => bl is [LABEL m] 
              (if List.null b then 
                bs else 
                ((l, [], rev b) :: bs)) 
              (SOME m)
          | _       => (if not (List.null (II.br i)) then
            bl is []       ((l, II.br i, rev (i :: b)) :: bs) NONE else
            bl is (i :: b) bs                        l))
        | bl [] [] bs l = bs
        | bl [] b  bs l = (l, [], rev b) :: bs in
      rev (bl meth [] [] NONE) end

    fun bbMapAddPrev map i prev = let
      val (insts, nexts, prevs) = valOf (IM.find (map, i)) in
      IM.insert (map, i, (insts, nexts, IS.add (prevs, prev))) end

    val bbs = genBlocks meth

    val (idBbMap, idLabNextsMap,labIdMap, _) = 
      List.foldl (fn ((labelop, nexts, insts), (idBbMap,idLabNextsMap ,labIdMap, i)) => (
        IM.insert (idBbMap, i, insts),
        IM.insert (idLabNextsMap, i, nexts),
        if isSome labelop then 
          IM.insert (labIdMap, valOf labelop, i) else
          labIdMap,
        i + 1)) (IM.empty,IM.empty, IM.empty, 0) bbs


    val bbmap = IM.foldli (fn (i, insts, bbmap) => let
      val nextLabs = valOf (IM.find (idLabNextsMap, i)) 
      val nextIds = List.map (fn lab => 
        valOf (IM.find (labIdMap, lab))) nextLabs in 
      IM.insert (bbmap, i, (insts, IS.fromList nextIds, IS.fromList [])) end) 
      IM.empty idBbMap 
    val bbmapref = ref bbmap
    val _ = IM.mapi (fn (i, (insts, nexts, _)) =>
      IS.map (fn next => (bbmapref := (bbMapAddPrev (! bbmapref) next i); 0))
      nexts) bbmap in

    ! bbmapref end

  and toString method = ListAux.toString method II.toString "\n"

end

structure BlockMap = struct

  structure IM = InterMethod
  open IntBinaryMapAux

  val i2s = Int.toString

  val fromMethod = IM.getBlockMap

  fun toString bbm = IntBinaryMapAux.toString bbm
    i2s (fn (insts, nexts, prevs) => let
      val instsStr = InterMethod.toString insts
      val nextsStr = IntBinarySetAux.toString nexts i2s "," 
      val prevsStr = IntBinarySetAux.toString prevs i2s "," in
      instsStr ^ 
      "\nNexts: " ^ nextsStr ^ 
      "\nPrevs: " ^ prevsStr end) "\n" "\n - - - - - - - \n"

end

