structure ValueEnvironment = struct

  open StringBinaryMap
  structure VS = ValueStructure
  structure VID = ValueIdentifier
  structure SM = StringBinaryMap
  structure AS = IntBinarySetAux

  type valenv = VS.valstr map

  fun instantiate ve is = SM.map (fn vs => VS.instantiate vs is) ve

  fun getAsstyset ve = 
    SM.foldl (fn (vs, set) => AS.union (VS.getAsstyset vs, set)) AS.empty ve

  fun toString tmap =
    SM.toString tmap VID.toString VS.toString " - " "\n"
end
