structure VartySet = struct
  structure IS = IntBinarySetAux
  structure VT = Varty
  open IS

  type vartyset = IS.set
  type sub = VT.varty * VT.varty
  type subseq = sub list

  fun toString s = "(" ^ IS.toString s VT.toString "," ^ ")"

  (* throw NotFound *)
  fun sub s (a, b) = IS.add ((IS.delete (s, a)), b)

  fun substitute set subseq = 
    List.foldl (fn (sb, set) => sub set sb) set subseq

  (* generate a subseq for s on excluding es *)
  fun aux (v :: vs) es =
    if IS.member (es, v) then 
      let val ex = IS.union (es, IS.fromList vs)
        val n = getExclusion ex 0 (fn x => x + 1)
        val es' = IS.add (es, n)
      in (v, n) :: aux vs es' end
    else aux vs (IS.add (es, v))
    | aux [] _ = []

  fun disjoint s1 s2 = let
    val sb = aux (IS.listItems s1) s2
    val s1' = substitute s1 sb
  in (s1', sb) end

end
