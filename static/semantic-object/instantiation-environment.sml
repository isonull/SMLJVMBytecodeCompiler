structure InstantiationEnvironment = struct

  open IntBinaryMapAux

  structure TS = TypeScheme
  structure TY = Type
  structure IM = IntBinaryMapAux

  type insenv = TS.ins map

  fun toString e = IM.toString e Assty.toString TS.toString ">" ";"

end
