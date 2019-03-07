structure InstantiationEnvironment = struct

  open IntBinaryMapAux

  structure TS = TypeScheme
  structure TY = Type
  structure IM = IntBinaryMapAux

  type insenv = TS.ins map

  fun insertInsseq map ((at, ts1) :: is) = let
    val ts2Op = find (map, at) in
    if Option.isSome ts2Op
    then let
      val ts2 = Option.valOf ts2Op
      val (ts, insmap) = TS.unify ts1 ts2
      val insseq = listItemsi insmap
      val map' = insert (map, at, ts) in
      insertInsseq map' (insseq @ is) end
    else insertInsseq (insert (map, at, ts1)) is end
    | insertInsseq map [] = map

  fun unify m1 m2 = let
    val insseq = listItemsi m2 in
    insertInsseq m1 insseq end

  and toString e = IM.toString e Assty.toString TS.toString ">" ";"

end
