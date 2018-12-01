structure Context = struct
  structure E = Environment
  structure T = TypeNameSet
  structure U = TypeVariableEnvironment
  structure I = InstantiationEnvironment
  structure TS = TypeScheme
  structure SE = StructureEnvironment
  structure TE = TypeEnvironment

  datatype env = datatype E.env
  type context = T.typenameset * U.tyvarenv * I.insenv * E.env

  (*fun instantiate (t, u, e) is = (t, u, E.instantiate e is)*)
  fun getValstr (_, _, _, e) lvid = E.getValstr e lvid
  fun getTystr (_, _, _, e) ltycon = E.getTystr e ltycon

  fun insenvAugment (t, u, i, e) i' =
    (t, u, I.unify i i', e)

  (* TODO implicit type variable *)
  fun valenvAugment (t, u, i, e) ve =
    (t, u, i, E.valenvAugment e ve)

  fun strenvAugment (t, u, i, e) se = let
    val t' = T.union (SE.getTynameset se, t)
    val e' = E.strenvAugment e se in
    (t', u, i, e') end

  fun tyenvAugment (t, u, i, e) te = let
    val t' = T.union (TE.getTynameset te, t)
    val e' = E.tyenvAugment e te in
    (t', u, i, e') end

  fun envAugment c (ENV (se, te, ve)) = 
    valenvAugment (tyenvAugment (strenvAugment c se) te) ve

  fun toString (t, u, i, e) = E.toString e
end
