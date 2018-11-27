functor OrdMapAuxFn (Map : ORD_MAP) = struct
  open Map
  fun toString map ktostr vtostr kvSpstr spstr =
    Map.foldri (fn (k, v, str) =>
      (ktostr k) ^ kvSpstr ^ (vtostr v) ^ spstr ^ str) "" map

  fun insertListPair map lst =
    List.foldl (fn ((k, v), m) => Map.insert (m, k, v)) map lst

  fun fromListPair lst = insertListPair Map.empty lst
end

structure StringBinaryMap = OrdMapAuxFn (BinaryMapFn (StringKey))
structure LabBinaryMap = struct
  structure LBM = OrdMapAuxFn (BinaryMapFn (LabKey))
  open LBM
  fun fromList lst = let
    val labs = List.tabulate (List.length lst, fn x => INTLAB x)
    val listPair = ListPair.zip (labs, lst)
  in fromListPair listPair end
end

structure SBM = StringBinaryMap
structure LBM = LabBinaryMap
