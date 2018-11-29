structure Context = struct
  structure E = Environment
  structure T = TypeNameBinarySet
  structure U = VartySet
  structure TS = TypeScheme

  type context = T.set * U.set * E.env

  fun instantiate (t, u, e) is = (t, u, E.instantiate e is)

  fun unify c t1 t2 = let
    val (t, is) = TS.unify t1 t2
    val c' = instantiate c is in
    (c', t) end
    
end
