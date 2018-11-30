structure TypeEnvironment = struct

  open StringBinaryMap
  structure TS = TypeStructure
  structure TF = TypeFunction
  structure ID = Identifier
  structure TYC = TypeConstructor
  structure SM = StringBinaryMap
  structure T = TypeNameSet

  type tyenv = TS.tystr map

  fun getTynameset te = foldli (fn (tc, (tf, ve), set) =>
    T.add (set, (([], tc), TF.getArity tf, false))) T.empty te

  fun toString te =
    SM.toString te TYC.toString TS.toString " - " "\n"
end
