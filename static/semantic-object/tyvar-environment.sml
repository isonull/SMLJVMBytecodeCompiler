structure TypeVariableEnvironment = struct

  open StringBinaryMap

  structure VS = VartySet
  structure IS = IntBinarySetAux

  type tyvarenv = Varty.varty map

  fun getVartyset map = foldl (fn (v, set) =>
    VS.add (set, v)) VS.empty map

  fun getRange map = foldl (fn (v ,set) => VS.add (set, v)) VS.empty map

  fun addTyvarseq map tyvarseq = let
    val range = VS.toIntSet (getRange map)
    val exs = IS.getExclusions range 0 (fn x => x + 1) (length tyvarseq)
    val (varseq, eqseq) = ListPair.unzip tyvarseq
    val tvExListPair = ListPair.zip (varseq, ListPair.zip (exs, eqseq))
    val map' = List.foldl (fn ((tv, vt), m) =>
      insert (m, tv, vt)) map tvExListPair in map' end

  val findOrigin = find
  val memberKeyOrigin = memberKey

  fun memberKey u (tv, eq) = memberKeyOrigin u tv

  fun find (u, (tv, eq)) = let val vt = findOrigin (u, tv) in
    if isSome vt then let 
      val (v, veq) = Option.valOf vt in 
      if veq = eq then SOME (v, veq) else
      raise Varty.Equality end
    else NONE end

end
