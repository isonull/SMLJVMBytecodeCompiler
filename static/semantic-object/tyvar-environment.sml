structure TypeVariableEnvironment = struct
  open StringBinaryMap
  structure VS = VartySet
  type tyvarenv = Varty.varty map
  
  fun getVartyset map = foldl (fn (v, set) =>
    VS.add (set, v)) VS.empty map

  fun getRange map = foldl (fn (v ,set) => VS.add (set, v)) VS.empty map

  fun addTyvarseq map tyvarseq = let
    val range = getRange map
    val exs = VS.getExclusions range 0 (fn x => x + 1) (length tyvarseq)
    val tvExListPair = ListPair.zip (tyvarseq, exs)
    val map' = List.foldl (fn ((tv, vt), m) => 
      insert (m, tv, vt)) map tvExListPair in map' end
end 
