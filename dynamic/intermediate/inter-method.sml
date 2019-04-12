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

  fun replaceLocs map method = List.map (II.replaceLocs map) method

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

    fun bbMapAddNext map i next = let
      val (insts, nexts, prevs) = valOf (IM.find (map, i)) in
      IM.insert (map, i, (insts, IS.add (nexts, next), prevs)) end

    fun lastInstSeq m = case (ListAux.last m) of
        RETURN _ => false
      | RAISE _ => false
      | _ => true

    val bbs = genBlocks meth

    val (idBbMap, idLabNextsMap,labIdMap, numbbs) = 
      List.foldl (fn ((labelop, nexts, insts), (idBbMap,idLabNextsMap ,labIdMap, i)) => (
        IM.insert (idBbMap, i, insts),
        IM.insert (idLabNextsMap, i, nexts),
        if isSome labelop then 
          IM.insert (labIdMap, valOf labelop, i) else
          labIdMap,
        i + 1)) (IM.empty,IM.empty, IM.empty, 0) bbs


    val bbmap = IM.foldli (fn (i, insts, bbmap) => let
      val nextLabs = valOf (IM.find (idLabNextsMap, i)) 
      val nextIds = (if lastInstSeq insts andalso (i+1)< numbbs then 
                        [i+1] else []) @ 
        (List.map (fn lab => 
          valOf (IM.find (labIdMap, lab))) nextLabs )
       in 
      IM.insert (bbmap, i, (insts, IS.fromList nextIds, IS.fromList [])) end) 
      IM.empty idBbMap 

    val bbmapref = ref bbmap

    val _ = IM.mapi (fn (i, (insts, nexts, _)) =>
      IS.map (fn next => (bbmapref := (bbMapAddPrev (! bbmapref) next i); 0))
      nexts) bbmap 
                                                   
     in

    ! bbmapref end

  and toString method = ListAux.toString method II.toString "\n"

end

structure BlockMap = struct

  structure IM = InterMethod
  open IntBinaryMapAux

  val isSome = Option.isSome
  val valOf = Option.valOf
  val getOpt = Option.getOpt


  val i2s = Int.toString

  val fromMethod = IM.getBlockMap

  fun toMethod bbmap = foldl (fn ((insts, _, _), m) => m @ insts) [] bbmap

  exception Ret of (int * int)

  fun pc2bpc bbmap pc = (foldli (fn (bi, (insts, _, _), pc') => let
    val bblen = length insts in
    if pc' + bblen > pc andalso pc' <= pc then raise (Ret (bi,pc - pc')) else
      pc' + bblen end) 0 bbmap; NONE)
    handle Ret i => SOME i

  (*fun hoist bbmap (bi, i) = let*)
    (*val (insts, _, prevs) = valOf (find (bbmap, bi)) in*)
    (*if i = 0 then *)

  fun toString bbm = IntBinaryMapAux.toString bbm
    i2s (fn (insts, nexts, prevs) => let
      val instsStr = InterMethod.toString insts
      val nextsStr = IntBinarySetAux.toString nexts i2s "," 
      val prevsStr = IntBinarySetAux.toString prevs i2s "," in
      instsStr ^ 
      "\nNexts: " ^ nextsStr ^ 
      "\nPrevs: " ^ prevsStr end) "\n" "\n - - - - - - - \n"

end

