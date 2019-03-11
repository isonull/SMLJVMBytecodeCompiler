structure TypeFunction = struct

  structure TY = Type
  structure VT = Varty
  structure VS = VartySet
  structure LA = ListAux
  structure TS = TypeScheme

  type tyfcn = VT.varty list * TY.ty

  exception UnmatchedArity

  fun fromTyname (tn as (_, a, _)) = let
    val is = List.tabulate (a, (fn x => (x, false)))
    val ts = List.map (fn x => TY.VARTY x) is in
    (is, TY.CONTY (ts, tn)) end

  (*calc_strictness no free variable in tyfcn *)
  fun disjointVartyset (vs, ty) evset = let
    val vsset = VS.fromList vs
    val (_, tvsubseq) = VS.disjoint vsset evset
    val vs' = LA.substitute vs tvsubseq
    val ty' = TY.bind ty
        (List.map (fn (a, b) => (a, TY.VARTY b)) tvsubseq) in
    (vs', ty') end

  fun apply tsseq (tf as (args, ty)) = let
    val tsseqDisjoint = TS.disjointList tsseq
    val (vs, evs) = List.foldl (fn ((vs, ty), (vss, evs)) =>
      (VS.union (vss, vs),
      VS.union (evs, TY.getVartyset ty))) (VS.empty, VS.empty) tsseqDisjoint
    val tfDisjoint = disjointVartyset tf evs
    val subseq = List.map (fn (arg, (vs, ty)) => (arg, ty))
      (ListPair.zip (args, tsseqDisjoint))
    val tyApp = TY.bind ty subseq in
    (vs, tyApp) end   

  fun getArity (ts, _) = List.length ts

  fun appTyname tsseq tn = let
    val tsseq' = TS.disjointList tsseq
    val vs = List.foldl (fn ((vs, _), set) =>
      VS.union (set, vs)) VS.empty tsseq'
    val tyseq = List.map (fn (_, t) => t) tsseq'
    val () = if TypeName.arity tn = length tsseq then ()
      else raise UnmatchedArity in
    (vs, TY.CONTY (tyseq, tn)) end

  fun toString (vs, t) =
    "A(" ^ (LA.toString vs VT.toString ",") ^ ")." ^ (TY.toString t)

end
