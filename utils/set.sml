functor OrdSetAuxFn (Set : ORD_SET) = struct

  open Set

  fun fromList ls = Set.addList (Set.empty, ls)

  fun deleteList (set, ls) = Set.difference (set, fromList ls)

  fun toString set tostr splitStr =
    ListAux.toString (listItems set) tostr splitStr

  fun unions sets =
    List.foldl (fn (s, us) => Set.union (s, us)) Set.empty sets

  fun tabulate (n, f) = let
    val ls = List.tabulate (n, f) in fromList ls end

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

  fun getDisjointSub s1 s2 init succ = let
    fun aux (v :: vs) es =
      if member (es, v) then
        let val ex = union (es, fromList vs)
          val n = getExclusion ex init succ
          val es' = add (es, n)
        in (v, n) :: aux vs es' end
      else aux vs (add (es, v))
      | aux [] _ = [] in
    aux (listItems s1) s2 end

end
