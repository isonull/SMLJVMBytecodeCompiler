structure Type : TYPE = struct

  open Identifier

  datatype ty =
    VARTY of tyvar |
    ROWTY of rowty |
    FUNTY of funty |
    CONTY of conty |
    (* assumed type is generated in pattern inference *)
    ASSTY of string

  withtype tyseq = ty list
  and tyvarseq = tyvar list
  and arity = int
  and eqty = bool
  and tyname = ltycon * arity * eqty
  and assty = string

  and tynameset = TynameBinarySet.set
  and tyvarset = StringBinarySet.set
  and asstyset = StringBinarySet.set

  and rowty = ty LabBinaryMap.map
  and funty = ty * ty
  and conty = tyseq * tyname

  and tyfcn = tyvarseq * ty
  and tysch = tyvarseq * ty

  type sub = ty * ty
  type subsr = sub list
  type subsl = sub list
  type ins = ty * ty
  type inssr = ins list
  type inssl = ins list

  exception UnificationFail
  exception WrongTypeForm

  val emptyT = TBS.empty
  val emptyU = SBS.empty
  val emptyA = SBS.empty

  fun modT a b = TBS.union (a, b)
  fun modU a b = SBS.union (a, b)
  fun modA a b = SBS.union (a, b)

  fun remVartyCon (VARTY x) = x
    | remVartyCon _ = raise WrongTypeForm

  fun remAsstyCon (ASSTY x) = x
    | remAsstyCon _ = raise WrongTypeForm

  fun getTyvarsInTy (VARTY v) = SBS.singleton v
    | getTyvarsInTy (ROWTY labmap) =
    LBM.foldr (fn (ty, set) => modU (getTyvarsInTy ty) set) emptyU labmap
    | getTyvarsInTy (FUNTY (ty, ty')) = modU (getTyvarsInTy ty) (getTyvarsInTy ty')
    | getTyvarsInTy (CONTY (tyseq, tyname)) =
    foldr (fn (ty, set) => modU (getTyvarsInTy ty) set) emptyU tyseq
    | getTyvarsInTy (ASSTY _) = SBS.empty

  fun getUinTysch (tvseq, ty) =
    SBS.deleteList (getTyvarsInTy ty, tvseq)

  (* only use for closed tyvar *)
  (* TODO: expand to longer string *)
  fun newTyvarseq arity = List.tabulate (arity,
    fn i => Char.toString (Char.chr ((Char.ord #"a") + i)))

  fun tynameInTy (name as (tycon, arity, _)) = let
    val tyvarseq = newTyvarseq arity
  in CONTY (map VARTY tyvarseq, name) end

  fun tynameInTyfcn (name as (tycon, arity, _)) = let
    val tyvarseq = newTyvarseq arity
  in (tyvarseq, CONTY (map VARTY tyvarseq, name)) end

  val tynameInTysch = tynameInTyfcn

  fun tySub (VARTY tyvar) (VARTY tyvar', ty) =
    if tyvar' = tyvar then ty else (VARTY tyvar)
    | tySub (ROWTY rowty) sub =
    ROWTY (LBM.map (fn ty => tySub ty sub) rowty)
    | tySub (ASSTY a) (ASSTY a', ty) = raise UnificationFail
    | tySub (FUNTY (ty, ty')) sub = FUNTY (tySub ty sub, tySub ty' sub)
    | tySub (CONTY (tyseq, tyname)) sub =
    CONTY (map (fn ty => tySub ty sub) tyseq, tyname)
    | tySub _ _ = raise UnificationFail

  fun tyIns (VARTY tyvar) (VARTY tyvar', ty) =
    raise UnificationFail
    | tyIns (ROWTY rowty) sub =
    ROWTY (LBM.map (fn ty => tyIns ty sub) rowty)
    | tyIns (ASSTY a) (ASSTY a', ty) =
    if a = a' then ty else ASSTY a
    | tyIns (FUNTY (ty, ty')) sub = FUNTY (tyIns ty sub, tyIns ty' sub)
    | tyIns (CONTY (tyseq, tyname)) sub =
    CONTY (map (fn ty => tyIns ty sub) tyseq, tyname)
    | tyIns _ _ = raise UnificationFail

  fun tySubsl ty subs = foldl (fn (sub, ty) => tySub ty sub) ty subs
  fun tySubsr ty subs = foldr (fn (sub, ty) => tySub ty sub) ty subs
  fun tyInssl ty inss = foldl (fn (ins, ty) => tyIns ty ins) ty inss
  fun tyInssr ty inss = foldr (fn (ins, ty) => tyIns ty ins) ty inss


  (* remove tv in tvs which is not in ty *)
  fun regTysch (tvs, ty) = let
    val tvset = getTyvarsInTy ty
  in (List.filter (fn tv => SBS.member (tvset, tv)) tvs, ty) end

  fun tyvarSubsByTySubs tysubs =
    List.map (fn (VARTY a, VARTY b) => (a, b) | _ => raise UnificationFail)
      (List.filter (fn (VARTY a, VARTY b) => true | _ => false) tysubs)

  fun tyschRebindl (vts, ty) tysubs closTvs = let
    val tvset = SBS.fromList vts
    val tysubs = List.filter
      (fn (r, _) => SBS.member (tvset, remVartyCon r)) tysubs
    val tvsubs = tyvarSubsByTySubs tysubs
    val tySubed = tySubsl ty tysubs
    val tvInTySubed = getTyvarsInTy tySubed
    val tvsSubed = ListAux.subsl vts tvsubs
    val tvsSubedSet = SBS.fromList tvsSubed
    val extTvs = List.filter 
          (fn x => (SBS.member (tvInTySubed, x))
       andalso not (SBS.member (tvsSubedSet, x))) closTvs 
  in regTysch (tvsSubed @ extTvs, tySubed) end

  fun tyschRebindr (vts, ty) tysubs closTvs =
    tyschRebindl (vts, ty) (List.rev tysubs) closTvs

  fun tyschSubsl (vts, ty) tysubs closTvs = let
    val tvset = SBS.fromList vts
    val tysubs = List.filter
      (fn (d, _) => not (SBS.member (tvset, remVartyCon d))) tysubs
    val tySubed = tySubsl ty tysubs
    val tvinTySubed = getTyvarsInTy tySubed
    val tvset = SBS.fromList vts
    val extvs = List.filter
      (fn x => (SBS.member (tvinTySubed, x))
    andalso not (SBS.member (tvset, x))) closTvs
  in (vts, tySubed) end

  fun isVarty (VARTY x) = true
    | isVarty _ = false
  fun isAssty (ASSTY x) = true
    | isAssty _ = false

  fun tyschInssl (tvs, ty) inss closTvs = let
    val tysubs = List.filter (fn (d, r) => isAssty d) inss
    val tyInsed = tyInssl ty inss
    val tvInTyInsed = getTyvarsInTy tyInsed
    val tvset = SBS.fromList tvs
    val extvs = List.filter
      (fn x => (SBS.member (tvInTyInsed, x))
   andalso not (SBS.member (tvset, x))) closTvs
    in (tvs @ extvs, tyInsed) end

  fun applyTyfcn (tyvarseq, ty) tyseq = let
    val vartyseq = map VARTY tyvarseq
    val subs = ListPair.zip (vartyseq, tyseq)
  in tySubsl ty subs end

  (* precondition: all elements are unique in the list *)
  (* head of the list is applied first *)
  fun genDisjoinSubsl (v :: vs) excl =
    if SBS.member (excl, v)
    then let val exclr = SBS.addList (excl, vs)
             val n = SBS.getUniqueString exclr
             val excl' = SBS.add (excl, n)
         in (VARTY v, VARTY n) :: genDisjoinSubsl vs excl' end
    else genDisjoinSubsl vs (SBS.add (excl, v))
    | genDisjoinSubsl [] _ = []

  fun disjoinTysch (vts, ty) (vts', ty') = let
    fun genExcl (vts, ty) (vts', ty') = let
      val vs = getTyvarsInTy ty
      val vs' = getTyvarsInTy ty'
      val closVs = SBS.fromList vts
      val openVs = SBS.difference (vs, closVs)
    in SBS.union (vs', openVs) end

    val subsl = genDisjoinSubsl vts (genExcl (vts, ty) (vts', ty'))
    val (vts, ty) = tyschRebindl (vts, ty) (subsl) []

    val subsl' = genDisjoinSubsl vts' (genExcl (vts', ty') (vts, ty))
  in ((vts, ty),
      tyschRebindl (vts', ty') (subsl') []) end

  fun tyschPairToFunTysch (tvs, ty) (tvs', ty') freeTyvarset = let
    val ((tvs, ty), (tvs', ty'))= disjoinTysch (tvs, ty) (tvs', ty')
  in regTysch (tvs @ tvs', FUNTY (ty, ty')) end

  fun getArgTyschInFunTysch (tvs, FUNTY (argty, resty)) = regTysch (tvs, argty)
    | getArgTyschInFunTysch _ = raise WrongTypeForm
  fun getResTyschInFunTysch (tvs, FUNTY (argty, resty)) = regTysch (tvs, resty)
    | getResTyschInFunTysch _ = raise WrongTypeForm
  fun splitFunTysch (tvs, FUNTY (argty, resty)) =
    (regTysch (tvs, argty), regTysch (tvs, resty))
    | splitFunTysch _ = raise WrongTypeForm

  (* precondition: tyvarseq disjoint *)
  (* precondition: no tyname allowed except initial tyname *)
  fun genTyUniSubsr tvs (VARTY tyvar) ty (subs, inss) false =
    genTyUniSubsr tvs (tySubsr (VARTY tyvar) subs) (tySubsr ty subs)
      (subs, inss) true

    | genTyUniSubsr tvs (VARTY tyvar) (VARTY tyvar') (subs, inss) true =
    if tyvar = tyvar'  then (subs, inss)
    else if SBS.member (tvs, tyvar)
         then (((VARTY tyvar), (VARTY tyvar')) :: subs, inss)
         else if SBS.member (tvs, tyvar')
              then (((VARTY tyvar'), (VARTY tyvar)) :: subs, inss)
              else raise UnificationFail

    | genTyUniSubsr tvs (VARTY tyvar) (ASSTY a) (subs, inss) true =
      if SBS.member (tvs, tyvar)
      then ((VARTY tyvar, ASSTY a) :: subs, inss)
      else (subs, (ASSTY a, VARTY tyvar) :: inss)

    | genTyUniSubsr tvs (VARTY tyvar) ty (subs, inss) true =
      if SBS.member (tvs, tyvar)
      then ((VARTY tyvar, ty) :: subs, inss)
      else raise UnificationFail

    | genTyUniSubsr tvs (ROWTY map) (ROWTY map') (subs, inss) subed =
    if LBM.numItems map = LBM.numItems map'
    then LBM.foldli (fn (key, ty, (ss, is)) =>
      let val ty' = LBM.find (map', key)
      in if Option.isSome ty'
         then ListAux.listPairAppend
                (genTyUniSubsr tvs ty (Option.valOf ty') (ss, is) false)
                (ss, is)
         else raise UnificationFail end) (subs, inss) map
    else raise UnificationFail

    | genTyUniSubsr tvs (FUNTY (ty1, ty2)) (FUNTY (ty1', ty2'))
                    (subs, inss) subed = let
    val (subs', inss') =
      ListAux.listPairAppend
        (genTyUniSubsr tvs ty1 ty1' (subs, inss) subed)
        (subs, inss)
    in ListAux.listPairAppend
         (genTyUniSubsr tvs ty2 ty2' (subs', inss') false)
         (subs', inss') end

    | genTyUniSubsr tvs (CONTY (tyseq, tyname)) (CONTY (tyseq', tyname'))
                    (subs, inss) subed =
      (* tyname realism is done before the calling *)
      if tyname = tyname' andalso length tyseq = length tyseq'
      then List.foldl (fn ((ty, ty'), subinss) =>
        ListAux.listPairAppend
          (genTyUniSubsr tvs ty ty' subinss false)
          subinss) (subs, inss) (ListPair.zip (tyseq, tyseq'))
      else raise UnificationFail

    | genTyUniSubsr tvs ty (VARTY tyvar) (subs, inss) subed =
      genTyUniSubsr tvs (VARTY tyvar) ty (subs, inss) subed

    | genTyUniSubsr tvs ty (ASSTY a) (subs, inss) subed =
      genTyUniSubsr tvs (ASSTY a) ty (subs, inss) subed

    | genTyUniSubsr tvs (ASSTY a) ty (subs, inss) false =
      genTyUniSubsr tvs (tySubsr (ASSTY a) subs) (tySubsr ty subs)
        (subs, inss) true

    (* instantiate the assumed type is the last choice *)
    (* valenv needs to be modified to change the vid type *)
    | genTyUniSubsr tvs (ASSTY a) ty (subs, inss) true =
      (subs, (ASSTY a, ty) :: inss)

    | genTyUniSubsr _ _ _ _ _ = raise UnificationFail

  fun genTyUniSubsl tvs ty ty' (subs, inss) subed = let
    val (subsr, inssr) = (genTyUniSubsr tvs ty ty' (subs, inss) subed)
  in (List.rev subsr, List.rev inssr) end

  (* postcondition: after substitution, both inputs are identical under
   * expansion *)
  fun genTyschUniSubsl (vts, ty) (vts', ty') = let
    val ((vts, ty), (vts', ty')) = disjoinTysch (vts, ty) (vts', ty')
    val validTyvarset = SBS.union (SBS.fromList vts, SBS.fromList vts')
  in (genTyUniSubsl validTyvarset ty ty' ([], []) false, 
      SBS.listItems validTyvarset) end

  fun tynameToString (ltycon, arity, eq) =
    (lidToString ltycon) ^ (Int.toString arity)

  fun tyschToString (tvs, ty) = "A" ^ (tyvarseqToString tvs) ^ "." ^ (tyToString ty)

  and tyfcnToString (tvs, ty) = "V" ^ (tyvarseqToString tvs) ^ "." ^ (tyToString ty)

  and tyToString (VARTY v) = "'" ^ v
    | tyToString (ROWTY map) =
    "{" ^ (LBM.toString map labToString tyToString "=" ",") ^ "}"
    | tyToString (CONTY (tys, tyname)) =
      (ListAux.toString tys tyToString ", ") ^ (tynameToString tyname)
    | tyToString (FUNTY (ty, ty')) = (tyToString ty) ^ "->" ^ (tyToString ty')
    | tyToString (ASSTY t) = "~" ^ t

end
