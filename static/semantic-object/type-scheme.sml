structure TypeScheme = struct

  structure TY = Type
  structure VS = VartySet
  structure IM = IntBinaryMapAux
  structure LM = LabBinaryMap

  datatype ty = datatype TY.ty

  type tysch = TY.vartyset * TY.ty
  type ins = TY.varty * tysch
  type insseq = ins list

  fun toString (vs, t) =
    "V" ^ (VS.toString vs) ^ "." ^ (TY.toString t)

  val wild = (VS.singleton 0, VARTY 0)

  fun fromTyname (tn as (_, a, _)) = let
    val is = List.tabulate (a, (fn x => x))
    val ts = List.map (fn x => TY.VARTY x) is in
    (VS.fromList is, TY.CONTY (ts, tn)) end

  (* remove bind varty not in the body *)
  fun reg (vs, t) = let
    val vst = TY.getVartyset t
    val vs' = VS.intersection (vs, vst) in
    (vs', t) end

  fun getOpenVartyset (vs, t) =
    VS.difference (TY.getVartyset t, vs)

  fun getAsstyset (vs, t) = TY.getAsstyset t

  fun disjointVartyset (vs, t) evs = let
    val ovs = getOpenVartyset (vs, t)
    val ex = VS.union (ovs, evs)
    val (vs', tvsubseq) = VS.disjoint vs ex
    val bndseq = map (fn (x, y) => (x, TY.VARTY y)) tvsubseq
    val t' = TY.bind t bndseq in
    reg (vs', t') end

  fun disjoint ts1 ts2 = let
    val ts1' = disjointVartyset ts1 (TY.getVartyset (#2 ts2))
    val ts2' = disjointVartyset ts2 (TY.getVartyset (#2 ts1')) in
    (ts1', ts2') end

  fun disjointList ts = let
    fun aux (t :: ts) cs  = let
      val ev = List.foldl (fn ((_, t), vs) =>
        VS.union (TY.getVartyset t, vs)) VS.empty (cs @ ts)
      val t' = disjointVartyset t ev in
      t' :: (aux ts (t' :: cs)) end
      | aux [] _ = [] in
    aux ts [] end

  fun instantiate ts tsis = let
    val tss = List.map (fn (a, ts) => ts) tsis
    val tstss' = disjointList (ts :: tss)

    (* disjoint the insmap and the input type scheme *)
    val (vs', t') = hd tstss'
    val tss' = tl tstss'
    val tmp = ListPair.zip (tsis, tss')
    (* new disjoint insmap *)
    val tsis' = map (fn ((a, ts), ts') => (a, ts')) tmp
    (* new disjoint insmap type version *)
    val tyis' = map (fn (a, (vs, t)) => (a, t)) tsis'
    val t'' = TY.mapInstantiate t' (IM.fromListPair tyis')
    (* take all tyvars *)
    val vs'' = VS.unions (List.map (fn (vs, t) => vs) tstss') in
    reg (vs'', t'') end

  fun getFunTysch ts1 ts2 = let
    val ((v1, t1), (v2, t2)) = disjoint ts1 ts2
    val v = VS.union (v1, v2)
    val t = FUNTY (t1, t2) in
    (v, t) end

  fun insertRowTysch rts lab ts = let
    fun aux ((v1, ROWTY (row, w)), (v2, t)) = (v1, row, v2, t, w)
      | aux _ = raise TY.WrongTypeForm "insertRowTysch"
    val (v1, row, v2, t, w) = aux (disjoint rts ts)
    val v = VS.union (v1, v2)
    val t = ROWTY (LM.insertUnoccupied row lab t, w) in
    (v, t) end

  fun noWildRowty (_, t) = TY.noWildRowty t

end

