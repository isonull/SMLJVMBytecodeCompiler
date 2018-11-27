structure StaticBasis : STATIC_BASIS = struct

  datatype idstat = VAL | CON | EXC

  open Identifier
  open Type

  datatype env = ENV of strenv * tyenv * valenv

  withtype typeseq = ty list
  and tyvarseq = tyvar list

  and valstr = tysch * idstat
  and valenv = valstr StringBinaryMap.map (* key is vid *)
  and tystr = tyfcn * valenv
  and tyenv = tystr StringBinaryMap.map (* key is tycon *)
  and strenv = env StringBinaryMap.map  (* key is strid *)

  and context = tynameset * tyvarset * asstyset * env

  exception ContextSearchFail

  val emptySE = SBM.empty
  val emptyTE = SBM.empty
  val emptyVE = SBM.empty

  val emptyE = ENV (emptySE, emptyTE, emptyVE)
  val emptyC = (emptyT, emptyU, emptyA, emptyE)

  fun EbySE strenv = ENV (strenv , emptyTE, emptyVE)
  fun EbyTE tyenv = ENV (emptySE, tyenv , emptyVE)
  fun EbyVE valenv = ENV (emptySE, emptyTE, valenv)

  fun CbyT tynameset = (tynameset, emptyU, emptyA, emptyE)
  fun CbyU tyvarset = (emptyT, tyvarset, emptyA, emptyE)
  fun CbyA asstyset = (emptyT, emptyU, asstyset, emptyE)
  fun CbyE env = (emptyT, emptyU, emptyA, env)

  fun SEofE (ENV (se, te, ve)) = se
  fun TEofE (ENV (se, te, ve)) = te
  fun VEofE (ENV (se, te, ve)) = ve

  fun UofC (tys, tvs, ass, env) = tvs
  fun TofC (tys, tvs, ass, env) = tys
  fun AofC (tys, tvs, ass, env) = ass
  fun EofC (tys, tvs, ass, env) = env

  fun strMapMod map map' = SBM.unionWith (fn (a, b) => b) (map, map)

  val modSE = strMapMod
  val modVE = strMapMod
  val modTE = strMapMod

  fun modE (ENV (se, te, ve)) (ENV (se', te', ve')) =
    ENV (strMapMod se se', strMapMod te te', strMapMod ve ve')

  fun modC (t, u, a, e) (t', u', a', e') =
    (modT t t', modU u u', modA a a', modE e e')

  fun mapT f = fn (tns, tvs, ats, env) => (f tns, tvs, ats, env)
  fun mapU f = fn (tns, tvs, ats, env) => (tns, f tvs, ats, env)
  fun mapA f = fn (tns, tvs, ats, env) => (tns, tvs, f ats, env)
  fun mapE f = fn (tns, tvs, ats, env) => (tns, tvs, ats, f env)

  fun mapSE f = fn (ENV (se, te, ve)) => ENV (f se, te, ve)
  fun mapTE f = fn (ENV (se, te, ve)) => ENV (se, f te, ve)
  fun mapVE f = fn (ENV (se, te, ve)) => ENV (se, te, f ve)

  fun getVSinE env vid = SBM.find ((VEofE env), vid)
  fun getTSinE env tycon = SBM.find ((TEofE env), tycon)
  fun getEinE env sid = SBM.find ((SEofE env), sid)

  fun pgetEinE env lidpre = SOME (
    foldr (fn (a, b) =>
            let val envOp = getEinE b a
            in if Option.isSome envOp then Option.valOf envOp
               else raise ContextSearchFail end) env lidpre)
  handle ContextSearchFail => NONE

  fun lgetXinE env (lidpre, id) getXinE = let
    val envOp = pgetEinE env lidpre
  in if Option.isSome envOp
     then getXinE env id
     else NONE end

  fun lgetVSinE env lvid = lgetXinE env lvid getVSinE
  fun lgetTSinE env ltycon = lgetXinE env ltycon getTSinE
  fun lgetEinE env lsid = lgetXinE env lsid getEinE

  fun lgetVSinC ctx lvid = lgetVSinE (EofC ctx) lvid
  fun lgetTSinC ctx ltycon = lgetTSinE (EofC ctx) ltycon
  fun lgetEinC ctx lsid = lgetEinE (EofC ctx) lsid


  (* TODO eqty *)
  fun getTinTE tyenv lidpre = let
    fun aux (tycon, (tyfcn as (tyvarseq, _), _)) =
      ((lidpre, tycon), length tyvarseq, false)
  in SBM.foldli (fn (tycon, tystr, set) => TBS.add (set, aux (tycon, tystr)))
      emptyT tyenv end

  (* TODO is this right? *)
  fun getTinE env = let
  fun aux (ENV (se, te, ve)) lidpre =
    SBM.foldli (fn (strid, env, set) =>
             modT (aux env (strid :: lidpre)) set) emptyT se
  in aux env [] end

  fun getUinVE strmap = SBM.foldr
    (fn ((tysch, _), set) => modU (getUinTysch tysch) set) emptyU strmap

  fun getUniqueAsstyByA set = let
    val uniqueStr = SBS.getUniqueString set
    val augA = SBS.add (set, uniqueStr)
    val assty = ASSTY (uniqueStr)
  in (augA, assty) end

  fun getUniqueAsstyByC (ctx as (t, u, a, e)) = let
    val (augA, assty) = getUniqueAsstyByA (AofC ctx)
    val augC = (t, u, augA, e)
  in (augC, assty) end

  fun getUniqueAssFuntyByC ctx = let
    val (augC1, argAssty) = getUniqueAsstyByC ctx
    val (augC2, resAssty) = getUniqueAsstyByC augC1
  in (augC2, FUNTY (argAssty, resAssty)) end

  fun expandTyByC ctx (CONTY (tyseq, (ltycon, arity, _))) = let
    fun filt (SOME x) = x
      | filt NONE = raise ContextSearchFail
    val (tyfcn, valenv) = filt (lgetTSinC ctx ltycon)
  in applyTyfcn tyfcn tyseq end
    | expandTyByC ctx (FUNTY (ty, ty')) =
    FUNTY (expandTyByC ctx ty, expandTyByC ctx ty')
    | expandTyByC ctx (ROWTY map) =
    ROWTY (LBM.map (fn ty => expandTyByC ctx ty) map)
    | expandTyByC _ (ASSTY a) = ASSTY a
    | expandTyByC _ (VARTY v) = VARTY v

  fun expandTyschByC ctx (tvs, ty) = (tvs, expandTyByC ctx ty)

  fun valstrInssl (tysch, idstat) inss closTvs = (tyschInssl tysch inss closTvs, idstat)

  fun valenvInssl map inss closTvs = 
    SBM.map (fn valstr => valstrInssl valstr inss closTvs) map

  fun envInssl (ENV (se, te, ve)) inss closTvs = 
    ENV (se, te, valenvInssl ve inss closTvs)

  fun insslC (tns, tvs, ass, env) inss closTvs = let
    val ass' = SBS.deleteList (ass, map remAsstyCon (#1 (ListPair.unzip inss)))
    val env' = envInssl env inss closTvs
  in (tns, tvs, ass', env') end

  fun expandAsstyTyschToFunAsstysch ctx ([], ASSTY a) = let
    val (augC, funAssty) = getUniqueAssFuntyByC ctx
    val inssl = [(ASSTY a, funAssty)]
    val insedC = insslC ctx inssl []
  in (insedC, ([], funAssty)) end
    | expandAsstyTyschToFunAsstysch _ _ = raise Type.WrongTypeForm

  (* TODO: optimise code *)
  fun unifyFcnArgTysch ctx fcnTysch argTysch = let
    val efcnTysch = expandTyschByC ctx fcnTysch
    val eargtysch = expandTyschByC ctx argTysch
    val (efcnargTysch, efcnresTysch) = splitFunTysch efcnTysch
    val ((subs, inss), closTvs) = genTyschUniSubsl efcnargTysch eargtysch
    val efcnresTysch' = tyschInssl (tyschRebindl efcnresTysch subs closTvs) inss closTvs
  in (insslC ctx inss closTvs, efcnresTysch') end

  fun unifyTysch ctx tysch tysch' = let
    val etysch = expandTyschByC ctx tysch
    val etysch' = expandTyschByC ctx tysch'
    val (tvs', _) = etysch'
    val ((subs, inss), closTvs) = genTyschUniSubsl etysch etysch'
    val resetysch = tyschInssl (tyschRebindl etysch subs closTvs) inss closTvs
  in (insslC ctx inss closTvs, resetysch) end

  fun idstatToString VAL = "v"
    | idstatToString CON = "c"
    | idstatToString EXC = "e"

  fun valstrToString (tysch, idstat) =
    (tyschToString tysch) ^ "," ^ (idstatToString idstat)

  and tystrToString (tyfcn, valenv) =
    (tyfcnToString tyfcn) ^ ",\n" ^ (valenvToString valenv)

  and valenvToString vmap =
    SBM.toString vmap idToString valstrToString " - " "\n"

  and tyenvToString tmap =
    SBM.toString tmap idToString tystrToString " - " "\n"

  and strenvToString smap = "STRUCT NOT DEVELOPPED"

  and envToString (ENV (se, te, ve)) =
    (strenvToString se) ^ "\n\n" ^
    (tyenvToString te) ^ "\n\n" ^
    (valenvToString ve) ^ "\n\n"

  fun contextToString (tnset, tvset, assty, env) = envToString env

  fun closTyschByU (tvseq, ty) tvset = let
    val tvsetInTysch = getUinTysch (tvseq, ty)
    val augTvs = SBS.listItems (SBS.difference (tvsetInTysch, tvset))
  in (tvseq @ augTvs, ty) end

  fun closVSbyU (tysch, idstat) tvset = (closTyschByU tysch tvset, idstat)

  fun closVEbyU valenv tvset =
    SBM.map (fn valstr => closVSbyU valstr tvset) valenv

  fun closVEbyC valenv ctx = closVEbyU valenv (UofC ctx)
end
