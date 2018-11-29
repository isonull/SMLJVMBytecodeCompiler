structure TypeScheme = struct

  structure TY = Type
  structure VS = VartySet

  type tysch = TY.vartyset * TY.ty
  type ins = TY.varty * tysch
  type insseq = ins list

  (* remove bind varty not in the body *)
  fun reg (vs, t) = let
    val vst = TY.getVartySet t
    val vs' = VS.intersection (vs, vst) in
    (vs', t) end

  fun getOpenVartySet (vs, t) =
    VS.difference (TY.getVartySet t, vs)

  fun disjoint ts1 ts2 = let
    fun aux (vs1, t1) (vs2, t2) = let
      val ovs1 = getOpenVartySet (vs1, t1)
      val ex1 = VS.union (ovs1, TY.getVartySet t2)
      val (vs1', subseq1) = VS.disjoint vs1 ex1
      val subseq1 = map (fn (x, y) => (x, TY.VARTY y)) subseq1
      val t1' = TY.bind t1 subseq1 in
      reg (vs1', t1') end

    val ts1' = aux ts1 ts2
    val ts2' = aux ts2 ts1' in
    (ts1', ts2') end

  fun disjointList (t1 :: t2 :: ts) = let
    val (t1', t2') = disjoint t1 t2 in
    t1' :: (disjointList (t2 :: ts)) end
    | disjointList [t] = [t]
    | disjointList [] = []

  fun instantiate ts tsis = let
    val tss = List.map (fn (a, ts) => ts) tsis
    val tstss' = disjointList (ts :: tss)
    val (vs', t') = hd tstss'
    val tss' = tl tstss'
    val tmp = ListPair.zip (tsis, tss')
    val tsis' = map (fn ((a, ts), ts') => (a, ts')) tmp
    val tyis' = map (fn (a, (vs, t)) => (a, t)) tsis'
    val t'' = TY.instantiate t' tyis'
    val vs'' = VS.unions (List.map (fn (vs, t) => vs) tstss') in
    reg (vs'', t'') end

  fun unify ts1 ts2 = let
    val ((vs1, t1), (vs2, t2)) = disjoint ts1 ts2
    val clos = VS.union (vs1, vs2)
    val (t, insseq) = TY.unify clos t1 t2
    val (clos', t) = reg (clos, t)
    val insseq' = map (fn (v, t) => (v, reg (clos', t))) insseq in
    ((clos', t), insseq') end

  fun toString (vs, t) =
    "V" ^ (VS.toString vs) ^ "." ^ (TY.toString t)
end
