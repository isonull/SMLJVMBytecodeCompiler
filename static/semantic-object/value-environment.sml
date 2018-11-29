structure ValueEnvironment = struct

  open StringBinaryMap
  structure VS = ValueStructure
  structure VID = ValueIdentifier
  structure SM = StringBinaryMap

  type valenv = VS.valstr map

  fun instantiate ve is = SM.map (fn vs => VS.instantiate vs is) ve

  fun toString tmap =
    SM.toString tmap VID.toString VS.toString " - " "\n"
end
