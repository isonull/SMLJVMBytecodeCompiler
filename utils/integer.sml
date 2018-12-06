structure IntKey = struct
  type ord_key = int
  val compare = Int.compare
end

structure IntBinarySet' = BinarySetFn (IntKey)
structure IntBinaryMap' = BinaryMapFn (IntKey)
structure IntBinaryMapAux = OrdMapAuxFn (IntBinaryMap')
structure IntBinarySetAux = IntBinaryMapAux.KeySet
