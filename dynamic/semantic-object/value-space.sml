structure ValueSpace = struct

  structure VIS = ValueIndexSet
  structure SM = StringBinaryMap
  structure IM = IntBinaryMapAux

  open StringBinaryMap

  type valspa = Value.valspa

  fun getIdset vs = SM.foldl (fn ((id, _), set) => VIS.add (set, id)) 
    VIS.empty vs

  fun dedup vs1 vs2 = let
    val idset1 = getIdset vs1
    val idset2 = getIdset vs2
    val sub = VIS.getDisjointSub idset1 idset2 0 (fn x => x + 1)
    val subMap = IM.fromListPair sub
    val vs2' = SM.map (fn (id, s) => let
      val sub = IM.find (subMap, id) in
      if sub = NONE
      then (id, s)
      else (Option.valOf sub, s) end) vs2 in
    vs2' end

end
