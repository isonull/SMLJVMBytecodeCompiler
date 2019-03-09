functor OrdMapAuxFn (Map : ORD_MAP) = struct
  open Map

  structure KeySet = OrdSetAuxFn(BinarySetFn (Key))
  structure KS = KeySet

  fun toString map ktostr vtostr kvSpstr spstr =
    Map.foldri (fn (k, v, str) =>
      (ktostr k) ^ kvSpstr ^ (vtostr v) ^ spstr ^ str) "" map

  fun insertListPair map lst =
    List.foldl (fn ((k, v), m) => Map.insert (m, k, v)) map lst

  fun fromListPair lst = insertListPair Map.empty lst

  fun keySet m = foldli (fn (k, _, s) => KS.add (s, k)) KeySet.empty m

  fun memberKey m k = KS.member (keySet m, k)

  fun isSubmap m1 m2 = KS.isSubset (keySet m1, keySet m2)

  fun excludeKeys m ks = KS.foldl (fn (k, ex) => 
    if memberKey m k then false else ex) true ks

  fun equalKeys m1 m2 = KS.equal (keySet m1, keySet m2)

  fun intersectKeyset map set  = KS.foldl (fn (k, m) => let
    val v = Map.find (map, k)
    fun aux (SOME v) = insert (m, k, v)
      | aux NONE = m in
    aux v end) Map.empty set

  (* TODO: review handle after patInf implementation *)
  fun removeKeyset map set = KS.foldl (fn (k, m) =>
    (#1 (Map.remove (m, k)))
    handle NotFound => m) map set

  fun removeList map lst = List.foldl (fn (k, m) =>
    (#1 (Map.remove (m, k)))
    handle NotFound => m) map lst

  fun modify m1 m2 = unionWith (fn (a, b) => b) (m1, m2)

  fun newKey m init succ = let
    val keyset = keySet m in
    KS.getExclusion keyset init succ end

end

