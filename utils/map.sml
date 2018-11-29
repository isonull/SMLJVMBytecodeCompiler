functor OrdMapAuxFn (Map : ORD_MAP) = struct
  open Map
  fun toString map ktostr vtostr kvSpstr spstr =
    Map.foldri (fn (k, v, str) =>
      (ktostr k) ^ kvSpstr ^ (vtostr v) ^ spstr ^ str) "" map

  fun insertListPair map lst =
    List.foldl (fn ((k, v), m) => Map.insert (m, k, v)) map lst

  fun fromListPair lst = insertListPair Map.empty lst
end

