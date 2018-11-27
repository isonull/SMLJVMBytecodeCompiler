functor OrdSetAuxFn (Set : ORD_SET) = struct
  open Set
  fun fromList ls = Set.addList (Set.empty, ls)
  fun removeList (set, ls) = Set.difference (set, fromList ls)
  fun toString set tostr =
    Set.foldr (fn (e, str) => (tostr e) ^ str ^ "|") "" set

end

structure StringBinarySet = OrdSetAuxFn (BinarySetFn (StringKey))
structure TynameBinarySet = OrdSetAuxFn (BinarySetFn (TynameKey))
structure LabBinarySet = OrdSetAuxFn (BinarySetFn (LabKey))

structure SBS = StringBinarySet
structure LBS = LabBinarySet
structure TBS = TynameBinarySet

