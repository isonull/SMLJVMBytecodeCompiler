functor OrdMapAuxFn (Map : ORD_MAP) = struct
  open Map

  structure KeySet = OrdSetAuxFn (BinarySetFn (Key))
  structure KS = KeySet
  fun toString map ktostr vtostr kvSpstr spstr =
    Map.foldri (fn (k, v, str) =>
      (ktostr k) ^ kvSpstr ^ (vtostr v) ^ spstr ^ str) "" map

  fun insertListPair map lst =
    List.foldl (fn ((k, v), m) => Map.insert (m, k, v)) map lst

  fun fromListPair lst = insertListPair Map.empty lst

  fun keySet m = foldli (fn (k, _, s) => KS.add (s, k)) KeySet.empty m

  fun memberKey (m, k) = KS.member (keySet m, k)
end

structure IntBinaryMapAux = OrdMapAuxFn (IntBinaryMap)
