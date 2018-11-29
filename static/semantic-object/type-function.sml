structure TypeFunction = struct

  structure TY = Type
  structure VT = Varty
  structure LA = ListAux
  type tyfcn = VT.varty list * TY.ty

  fun fromTyname (tn as (_, a, _)) = let
    val is = List.tabulate (a, (fn x => x))
    val ts = List.map (fn x => TY.VARTY x) is in
    (is, TY.CONTY (ts, tn)) end

  fun toString (vs, t) = 
    "A(" ^ (LA.toString vs VT.toString ",") ^ ")." ^ (TY.toString t)

end
