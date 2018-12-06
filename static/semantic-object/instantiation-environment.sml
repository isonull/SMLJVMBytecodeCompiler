structure InstantiationEnvironment = struct

  structure TS = TypeScheme
  structure TY = Type
  structure IM = IntBinaryMapAux

  open IntBinaryMapAux

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
    (print ((toString m1) ^ " --- I.1\n" ^
            (toString m2) ^ " --- I.2\n" ^
            (toString (insertInsseq m1 insseq)) ^ " --- I.3\n")); insertInsseq m1 insseq end

  and toString e = IM.toString e Assty.toString TS.toString ">" ";"

end
