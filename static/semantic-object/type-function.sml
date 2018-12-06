structure TypeFunction = struct

  structure TY = Type
  structure VT = Varty
  structure VS = VartySet
  structure LA = ListAux
  structure TS = TypeScheme

  type tyfcn = VT.varty list * TY.ty

  exception UnmatchedArity

  fun fromTyname (tn as (_, a, _)) = let
    val is = List.tabulate (a, (fn x => x))
    val ts = List.map (fn x => TY.VARTY x) is in
    (is, TY.CONTY (ts, tn)) end

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
