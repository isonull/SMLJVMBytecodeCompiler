functor OrdSetAuxFn (Set : ORD_SET) = struct

  open Set

  fun fromList ls = Set.addList (Set.empty, ls)
  fun deleteList (set, ls) = Set.difference (set, fromList ls)
  fun toString set tostr splitStr =
    Set.foldr (fn (e, str) => (tostr e) ^ splitStr ^ str ) "" set

  fun unions sets =
    List.foldl (fn (s, us) => Set.union (s, us)) Set.empty sets

  fun getExclusion set init succ = let
    fun aux s = if Set.member(set, s) then aux (succ s) else s
  in aux init end

  fun getExclusions set init succ n = let
    fun aux s 0 = []
      | aux s n = if n < 0 then raise Size else
      if Set.member (set, s) then aux (succ s) n else
        s :: (aux (succ s) (n - 1)) in
    aux init n end

  fun substitute set subs = List.foldl (fn ((d, r), set) => 
    Set.map (fn x => if Set.Key.compare (x, d) = EQUAL 
                     then r else x) set) set subs
end

