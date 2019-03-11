structure Varty = struct

  structure SA = StringAux

  type varty = int * bool

  exception Equality

  fun toString (n, eq) = let
    fun aux 0 = "a"
      | aux n = SA.succ (aux (n - 1))
  in (if eq then "''" else "'") ^ (aux n) end

end

structure VartyKey : ORD_KEY = struct

  type ord_key = Varty.varty

  fun compare ((_, true), (_, false)) = GREATER
    | compare ((_, false), (_, true)) = LESS
    | compare ((v1, _), (v2, _)) = Int.compare (v1,v2)

end

structure VartySet = struct

  structure VS = OrdSetAuxFn (BinarySetFn (VartyKey))
  open VS

  structure VT = Varty
  structure IM = IntBinaryMapAux
  structure IS = IntBinarySetAux

  type vartyset = VS.set
  type sub = VT.varty * VT.varty
  type subseq = sub list

  fun toString s = "(" ^ VS.toString s VT.toString "," ^ ")"

  (* throw NotFound *)
  fun sub s (a, b) = VS.add ((VS.delete (s, a)), b)

  (* Written in parent *)
  (*fun substitute set subseq =*)
    (*List.foldl (fn (sb, set) => sub set sb) set subseq*)

  fun toIntSet set = VS.foldl (fn ((v, eq), is) => IS.add (is, v)) IS.empty set
  fun toEqmap set = 
    VS.foldl (fn ((v, eq), m) => IM.insert (m, v, eq)) IM.empty set

  (* Written in parent *)
  (* generate a subseq for s on excluding es *)
  (*fun aux (v :: vs) es =*)
    (*if IS.member (es, v) then*)
      (*let val ex = IS.union (es, IS.fromList vs)*)
        (*val n = getExclusion ex 0 (fn x => x + 1)*)
        (*val es' = IS.add (es, n)*)
      (*in (v, n) :: aux vs es' end*)
    (*else aux vs (IS.add (es, v))*)
    (*| aux [] _ = []*)

  fun disjoint s1 s2 = let
    val vs1 = toIntSet s1
    val vs2 = toIntSet s2
    val em1 = toEqmap s1
    val sb = IS.getDisjointSub vs1 vs2 0 (fn x => x + 1)
    val vsb = List.map (fn (s, t) => let
      val eq = Option.valOf (IM.find (em1, s)) in ((s,eq), (t,eq)) end) sb
    val s1' = substitute s1 vsb
  in (s1', vsb) end

end
