structure Environment = struct
  structure SE = StructureEnvironment
  structure TE = TypeEnvironment
  structure VE = ValueEnvironment
  structure SM = StringBinaryMap
  structure SID = StructureEnvironment

  datatype env = datatype SE.env

  fun instantiate (ENV (se, te, ve)) is = ENV (se, te, VE.instantiate ve is)

  fun toString (ENV (se, te, ve)) =
    (SE.toString se) ^ "\n\n" ^
    (TE.toString te) ^ "\n\n" ^
    (VE.toString ve) ^ "\n\n"
end
