structure StructureEnvironment = struct

  structure VE = ValueEnvironment
  structure TE = TypeEnvironment
  open StringBinaryMap

  datatype env = ENV of strenv * TE.tyenv * VE.valenv
  withtype strenv = env map

  fun toString se = "STRUCT NOT DEVELOPPED"

end
