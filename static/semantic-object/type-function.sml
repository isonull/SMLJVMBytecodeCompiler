structure TypeFunction = struct

  structure TY = Type
  structure VT = Varty
  structure LA = ListAux
  type tyfcn = VT.varty list * TY.ty

  fun toString (vs, t) = 
    "A" ^ (LA.toString vs VT.toString ",") ^ "." ^ (TY.toString t)

end
