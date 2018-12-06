structure LongTypeConstructorKey = struct

  structure LTYC = LongTypeConstructor

  type ord_key = LTYC.ltycon
  val compare = LTYC.compare

end

structure LongTypeConstructorMap = OrdMapAuxFn (BinaryMapFn (LongTypeConstructorKey))

structure TypeNameEnvironment = struct
  open LongTypeConstructorMap

  structure TS = TypeNameBinarySet
  type tynameenv = TypeName.tyname map

  fun fromTynameset tnset = TS.foldl (fn (tn as (ltycon, _, _), map) =>
    insert (map, ltycon, tn)) empty tnset

end
