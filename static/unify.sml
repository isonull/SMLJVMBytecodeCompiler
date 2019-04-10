structure Unify = struct

  structure TY = Type
  structure VS = VartySet
  structure IM = IntBinaryMapAux
  structure LM = LabBinaryMap
  structure E = Environment
  structure IS = IntBinarySetAux
  structure LA = ListAux
  structure LPA = ListPairAux
  structure TN = TypeName
  structure VS = VartySet
  structure AS = IntBinarySetAux
  structure TS = TypeScheme
  structure I = InstantiationEnvironment
  structure TF = TypeFunction
  structure LTCM = LongTypeConstructorMap
  structure LS = LabBinarySet

  datatype ty = datatype TY.ty

  val valOf = Option.valOf
  val isSome = Option.isSome

  fun gs tm cs t1 t2 = let

  (* serves as occupied vartys for expand rowty *)
  val cs = ref cs
  val vartyset = ref (IS.union (VS.toIntSet (! cs), 
    IS.union (VS.toIntSet (TY.getVartyset t1), VS.toIntSet (TY.getVartyset t2))))

  fun newex () = let
    val ne = IS.getExclusion (! vartyset) 0 (fn x => x+1) in
    cs := VS.add (! cs, (ne, false));
    vartyset := IS.add (! vartyset, ne); (ne, false) end

  (* rowTy, newlabels -> update exclude vartyset, filled rowty *)
  fun fillRowty rowty newlabs = if LM.excludeKeys rowty newlabs then
    LS.foldl (fn (lab, row) =>
      LM.insert (row, lab, VARTY (newex ())) ) rowty newlabs
    else raise TY.UnifyFail "ASSERTION FAIL"

  fun matchWildRowty (r1, w1) (r2, w2) = let
    val ks1 = LM.keySet r1
    val ks2 = LM.keySet r2 in (case (w1, w2) of
      (false, false) => if LS.equal (ks1, ks2) then ((r1, false), (r2, false)) else
      raise TY.UnifyFail "DIFFERENT LABSETS IN FIRM TYROWS"
    | (false, true) => if LS.isSubset (ks2, ks1) then let
      val r2' = fillRowty r2 (LS.difference (ks1, ks2)) in 
      ((r1, false), (r2', false)) end else
      raise TY.UnifyFail "EXTRA LABS IN FIRM TYROWS TO WILD TYROWS"
    | (true, false) => let 
      val (a, b) = matchWildRowty (r2, w2) (r1, w1) in (b, a) end
    | (true, true) => let
      val r1' = fillRowty r1 (LS.difference (ks2, ks1))
      val r2' = fillRowty r2 (LS.difference (ks1, ks2)) in 
      ((r1', true), (r2', true)) end) end

  fun isBas t1 t2 = (TY.isAssty) t1 orelse (TY.isVarty t1)
             orelse (TY.isAssty) t2 orelse (TY.isVarty t2)

  fun getTf n = (Option.valOf o LTCM.find) (tm, TN.ltycon n)
      handle Option => raise TY.UnifyFail "TYNAME NOT DEFINED"

  fun appTf ts tf = #2 (TF.apply (List.map (fn t => TS.reg (! cs,t)) ts) tf)

  fun expConty (ts, tn) = let
    val res = appTf ts (getTf tn)
    fun recCheck (t as CONTY (ts', tn')) =
      if TN.equal tn tn' then NONE else SOME t
      | recCheck t = SOME t in recCheck res end

  fun bas t1 t2 s = let
    val t1' = TY.substitute t1 (List.rev s)
    val t2' = TY.substitute t2 (List.rev s)

    fun v2vAux (tv1 as VARTY (v1, eq1)) (tv2 as VARTY (v2, eq2)) = 
      if v1 = v2 then (
        if eq1 = eq2 then
          [] else
          raise TY.UnifyFail "ASSERTION FAIL !!! : THE SAME TYVAR DIFF EQ") else (
        case (VS.member (!cs, (v1, eq1)), VS.member (!cs, (v2, eq2))) of 
          (true, true) => (case (eq1, eq2) of
            (true, false) => [(tv2, tv1)]
          | _ => [(tv1, tv2)]) 
        | (true, false) => (case (eq1, eq2) of
            (true, false) => raise TY.UnifyFail "FREE TYVAR EQ to NFREE TYVAR NEQ"
          | _ => [(tv1, tv2)])
        | (false, true) => (case (eq1, eq2) of
            (false, true) => raise TY.UnifyFail "FREE TYVAR EQ to NFREE TYVAR NEQ"
          | _ => [(tv2, tv1)])
        | (false, false) => raise TY.UnifyFail "OPEN TYVARS NOT EQ"
        )

    fun basaux (VARTY v1) (VARTY v2) =
      ((v2vAux (VARTY v1) (VARTY v2)) @ s, VARTY v1)

      | basaux (VARTY v) (ASSTY a) =
      if VS.member (! cs, v)
      then ((VARTY v, ASSTY a) :: s, ASSTY a)
      else ((ASSTY a, VARTY v) :: s, ASSTY a)

      | basaux (ASSTY a) (VARTY v) =
      basaux (VARTY v) (ASSTY a)

      | basaux (ASSTY a1) (ASSTY a2) =
      if a1 = a2 then (s, ASSTY a1) else 
        if a1 < a2 then ((ASSTY a1, ASSTY a2) :: s, ASSTY a2)
        else ((ASSTY a2, ASSTY a1) :: s, ASSTY a1)

      | basaux (VARTY v) ty =
      if TY.ctva ty (VARTY v)
      then raise TY.UnifyFail "CIRCULAR VARTY" else
        if VS.member (! cs, v)
        then ((VARTY v, ty) :: s, ty)
        else raise TY.UnifyFail "OPEN VARTY"

      | basaux (ASSTY a) ty =
      if TY.ctva ty (ASSTY a)
      then raise TY.UnifyFail "CIRCULAR ASSTY"
      else ((ASSTY a, ty) :: s, ASSTY a)

      | basaux ty (VARTY v) =
      basaux (VARTY v) ty

      | basaux ty (ASSTY a) =
      basaux (ASSTY a) ty in
    if isBas t1' t2' then
      basaux t1' t2' else
      aux t1' t2' s end

  and aux t1 t2 s = if isBas t1 t2 then bas t1 t2 s else (case (t1, t2) of
      (FUNTY (a1, r1), FUNTY (a2, r2)) => let
      val (s' , t1) = aux a1 a2 s
      val (s'', t2) = aux r1 r2 s'
    in (s'' @ (s' @ s), FUNTY (t1, t2)) end

    | (ROWTY (r1, w1), ROWTY (r2, w2)) => let
      val ((r1, w1), (r2, w2)) = matchWildRowty (r1, w1) (r2, w2) in
    if LM.numItems r1 = LM.numItems r2
    then let val (s, r) = LM.foldli (fn (k, t1, (s, r)) => let
      val t2 = Option.valOf (LM.find (r2, k))
        handle Option.Option => ((
        TIO.println "UNIFY FAILED BETWEEN";
        TIO.println (TY.toString (ROWTY (r1, w1)));
        TIO.println (TY.toString (ROWTY (r2, w2))));
        raise TY.UnifyFail "LAB NOT FOUND")
      val (saux, t) = (aux t1 t2 s) in 
      (saux @ s, LM.insert (r, k, t)) end) (s, LM.empty) r1 in
      (s, ROWTY (r, w1 andalso w2)) end
    else raise TY.UnifyFail "ROWTY DIFFERENCE SIZE" end

    | (CONTY (ts1, n1), CONTY (ts2, n2)) =>
    if TN.equal n1 n2 then let
      val tp = ListPair.zip (ts1, ts2)
      val (s, ts) = List.foldl (fn ((t1, t2), (s, ts)) => let
        val (saux, t) = (aux t1 t2 s) in 
        (saux @ s, ts @ [t]) end) (s, []) tp in 
      (s, CONTY (ts, n1)) end
    else let
      val t1op = expConty (ts1, n1)
      val t2op = expConty (ts2, n2) in
      if isSome t1op orelse isSome t2op then
        aux (Option.getOpt (t1op, CONTY (ts1, n1)))
            (Option.getOpt (t2op, CONTY (ts2, n2))) s
      else raise TY.UnifyFail "BAS CONTY PAIR DOES NOT UNIFY" end

    | (CONTY (ts, n), t2) => (aux ((valOf o expConty) (ts, n)) t2 s
      handle Option => raise TY.UnifyFail
        "BAS CONTY AND OTHER TY DOES NOT UNIFY")

    | (t1, t2 as (CONTY _)) => aux t2 t1 s) in let
    val (s, t) = aux t1 t2 [] in
    (List.rev s, ! cs, t) end end

  (* generate type instantiation from substitution sequence*)
  (* useless in this case, we do not want to instantiate until the vrow retire *)
  (*fun ifs ((ASSTY a, ty) :: ss) =*)
    (*(a, TY.substitute ty ss) :: (ifs ss)*)
    (*| ifs (_ :: ss) = ifs ss*)
    (*| ifs [] = []*)

  (* truncate the free variable binding
   * make instantiation immediate and so ins and bnd separatable*)
  fun truncateSubseq ((t1, t2) :: ss) =
    (t1, TY.bind t2 (TY.bndseqFromSubseq ss)) :: (truncateSubseq ss)
    | truncateSubseq [] = []

  fun subseqToString seq = ListAux.toString seq (fn (a, b) => (TY.toString a)
    ^ (TY.toString b)) "||"

  fun unifyTy tm cs t1 t2 = let
    val (subseq, newcs, newty) = gs tm cs t1 t2
    val tsubseq = truncateSubseq subseq
    val insseq = TY.insseqFromSubseq tsubseq
    val bndseq = TY.bndseqFromSubseq tsubseq
    val t = TY.bind newty bndseq
  in (t, insseq, newcs) end
  handle TY.UnifyFail s => (
    TIO.println s;
    TIO.println (TY.toString t1);
    TIO.println (TY.toString t2);
    raise TY.UnifyFail s
  )

  fun unifyTs tfmap ts1 ts2 = let
    val ((vs1, t1), (vs2, t2)) = TS.disjoint ts1 ts2
    val clos = VS.union (vs1, vs2)
    val (t, insseq, newclos) = unifyTy tfmap clos t1 t2
    val (clos', t) = TS.reg (newclos, t)
    val insseq' = map (fn (v, t) => (v, TS.reg (clos, t))) insseq
    val insmap = IM.fromListPair insseq' in
    ((clos', t), insmap) end

  handle TY.UnifyFail s => (
    TIO.println s;
    TIO.println (TS.toString ts1);
    TIO.println (TS.toString ts2);
    raise TY.UnifyFail s)

  fun unifyI tm m1 m2 = let
    fun insertInsseq map ((at, ts1) :: is) = let
      val ts2Op = I.find (map, at) in
      if Option.isSome ts2Op
      then let
        val ts2 = Option.valOf ts2Op
        val (ts, insmap) = unifyTs tm ts1 ts2
        val insseq = I.listItemsi insmap
        val map' = I.insert (map, at, ts) in
        insertInsseq map' (insseq @ is) end
      else insertInsseq (I.insert (map, at, ts1)) is end
      | insertInsseq map [] = map in
    insertInsseq m1 (I.listItemsi m2) end

  fun getLvidTyfcnMap (_,_,e) = E.getLvidTyfcnMap e

  fun tsUnify c ts1 ts2 = unifyTs (getLvidTyfcnMap c) ts1 ts2
  fun iUnify c i1 i2 = unifyI (getLvidTyfcnMap c) i1 i2

end
