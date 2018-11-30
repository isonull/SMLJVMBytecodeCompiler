structure StructureEnvironment = struct

  structure VE = ValueEnvironment
  structure TE = TypeEnvironment
  structure T = TypeNameSet
  open StringBinaryMap

  datatype env = ENV of strenv * TE.tyenv * VE.valenv
  withtype strenv = env map

  fun getTynameset se = foldli (fn (sid, env, set) => 
      T.union (T.prefixSid (getTynamesetEnv env) sid, set)) T.empty se

  and getTynamesetEnv (ENV (se, te, _)) = let
    val setn = getTynameset se
    val tetn = TE.getTynameset te in
    T.union (setn, tetn) end

  fun toString se = "STRUCT NOT DEVELOPPED"

end
