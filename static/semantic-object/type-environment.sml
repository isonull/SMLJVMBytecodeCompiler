structure TypeEnvironment = struct

  open StringBinaryMap
  structure TS = TypeStructure
  structure ID = Identifier
  structure TYC = TypeConstructor
  structure SM = StringBinaryMap

  type tyenv = TS.tystr map

  fun toString te =
    SM.toString te TYC.toString TS.toString " - " "\n"
end
