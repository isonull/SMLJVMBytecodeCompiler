structure Context = struct

  structure E = Environment
  structure T = TypeNameEnvironment
  structure U = TypeVariableEnvironment
  structure I = InstantiationEnvironment
  structure TS = TypeScheme
  structure SE = StructureEnvironment
  structure TE = TypeEnvironment
  structure VE = ValueEnvironment

  datatype env = datatype E.env

  type context = T.tynameenv * U.tyvarenv * I.insenv * E.env

  val empty = (T.empty, U.empty, E.empty)

  fun getTyname (t, u, e) ltycon = T.find (t, ltycon)

  fun getVarty (t, u, e) tyvar = U.find (u, tyvar)

  fun getVartyset (t, u, e) = U.getVartyset u

  fun addTyvarseq (t, u, e) tyvarseq = (t, U.addTyvarseq u tyvarseq, e)

  fun isTyvar (t, u, e) tyvar = U.memberKey u tyvar

  (*fun instantiate (t, u, e) is = (t, u, E.instantiate e is)*)
  fun getAsstyset (t, u, e) = E.getAsstyset e

  fun getValstr (_, _, e) lvid = E.getValstr e lvid

  fun getTystr (_, _, e) ltycon = E.getTystr e ltycon

  fun getEnv (_, _, e) (pre, sid) = let
    fun aux (ENV e) (sid :: sidseq) =
      aux (Option.valOf (SE.find ((#1 e), sid))) sidseq
      | aux _ [] = e in
    aux e (pre @ [sid]) end

  (* TODO implicit type variable *)
  fun valenvAugment (t, u, e) ve =
    (t, u, E.valenvAugment e ve)

  fun strenvAugment (t, u, e) se = let
    val t' = T.modify t (T.fromTynameset (SE.getTynameset se))
    val e' = E.strenvAugment e se in
    (t', u, e') end

  fun tyenvAugment (t, u, e) te = let
    val t' = T.modify t (T.fromTynameset (TE.getTynameset te))
    val e' = E.tyenvAugment e te in
    (t', u, e') end

  fun envAugment c (ENV (se, te, ve)) =
    valenvAugment (tyenvAugment (strenvAugment c se) te) ve

  fun tynameenvAugment (t, u, e) tynameenv = (T.modify t tynameenv, u, e)

  fun closTysch c (vs, ty) = let
    val vartysetC = getVartyset c
    val openVartysetTs = TS.getOpenVartyset (vs, ty)
    val vs' = VartySet.union
      (VartySet.difference (openVartysetTs, vartysetC),
        vs) in
    (vs', ty) end

  fun closValenv c valenv = VE.map (fn (ts ,is) => (closTysch c ts, is)) valenv

  (* apply the type function to all ungrounded tynames for unification *)
  (* TODO: heuristic expanding? *)
  (*fun tsGround c (vs, TY.CONTY (tyseq, ltycon)) = let*)
    (*val tystr = Option.valOf (getTystr c ltycon)*)
      (*handle Option => raise Option*)
    (*val tsseq = List.map (fn ty => TS.reg (vs, ty)) tyseq*)
    (*val res = TF.apply tsseq (#tyfcn tystr) in*)
    (*TS.reg res end*)

  fun toString (t, u, e) = E.toString e
end
