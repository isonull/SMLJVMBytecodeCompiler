structure TypeEnvironment = struct

  structure TS = TypeStructure
  structure TF = TypeFunction
  structure ID = Identifier
  structure TYC = TypeConstructor
  structure SM = StringBinaryMap
  structure T = TypeNameSet

  open StringBinaryMap

  type tyenv = TS.tystr map

  fun getTynameset te = foldli (fn (tc, (tf, ve), set) =>
    T.add (set, (([], tc), TF.getArity tf, false))) T.empty te

  fun getVidTyfcnMap te = foldli (fn (tc, (tf, _), map) => 
    SM.insert (map, tc, tf)) SM.empty te

  fun toString te =
    SM.toString te TYC.toString TS.toString " - " "\n"
end
