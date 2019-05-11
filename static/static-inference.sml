structure StaticInference = struct


  structure C = Context
  structure CS = CoreSyntaxTree
  structure ISB = InitialStaticBasis

  structure TS = TypeScheme
  structure TF = TypeFunction
  structure TY = Type

  structure TE = TypeEnvironment
  structure VE = ValueEnvironment
  structure SE = StructureEnvironment
  structure E = Environment

  structure AS = AsstySet
  structure VS = VartySet
  structure T = TypeNameEnvironment
  structure I = InstantiationEnvironment
  structure IM = IntBinaryMapAux
  structure IS = IntBinarySetAux

  structure IST = IntermediateSyntaxTree

  exception StaticInferenceFail of string

  datatype idstat    = datatype IdentifierStatus.idstat
  datatype scon      = datatype CST.scon
  datatype atexp     = datatype CST.atexp
  datatype exp       = datatype CST.exp
  datatype dec       = datatype CST.dec
  datatype valbind   = datatype CST.valbind
  datatype exbindele = datatype CST.exbindele
  datatype atpat     = datatype CST.atpat
  datatype pat       = datatype CST.pat
  datatype ty        = datatype CST.ty

  fun patsComp [AT_PAT (LVID_ATPAT _)] c = true
    | patsComp [AT_PAT WILD_ATPAT]     c = true
    | patsComp _ _ = false

  fun matchComp (match : CST.match) c = patsComp (map #1 match) c

  fun vrowComp (vrow : CST.vrow) c = patsComp (map #1 vrow) c
  
  val asstySet = ref (AS.empty : AS.set)
  (* DAG graph *)

  fun tsUnify c ts1 ts2 = (Unify.tsUnify c ts1 ts2 asstySet)
  fun iUnify c i1 i2 = Unify.iUnify c i1 i2 asstySet

  (* create a new assty with optional parent p *)
  fun newAssty p = let
    val new = AS.getNewAssty (! asstySet) in
    asstySet := AS.add (! asstySet, new);
    new end

  fun rmAsstys set = (
    asstySet := AS.difference (! asstySet, set)
    )

  (* split the insmap to two part, one is used for instantiation
   * and the second part is instantiated and returned to lower layer *)
  fun getInsResI i c = let
    (* keep the key in the context *)
    val keepkeys = C.getAsstyset c
    val iIns = I.removeKeyset i keepkeys
    val iRes = I.intersectKeyset i keepkeys
    val iRes = I.map (fn ts => TS.instantiate ts iIns) iRes in
    rmAsstys (I.keySet iIns);
    (iIns, iRes) end

  (* instantiate the valenv with insmap only on keys
   * remove the instantiated key from asstySet *)
  fun veInstantiateKeys ve i c = let
    val (iIns, iRes) = getInsResI i c
    val veIns = VE.instantiate ve iIns in
    if VE.noWildRowty veIns then
    (veIns, iRes) else
    raise StaticInferenceFail "WILD ROWTY IN VALBIND" end

  fun expsynInstantiateKeys syn i c = let
    val (iIns, _) = getInsResI i c
    val synIns = IST.insExp iIns syn in synIns end

  fun patsynInstantiateKeys syn i c = let
    val (iIns, _) = getInsResI i c
    val synIns = IST.insPat iIns syn in synIns end

  fun tsInstantiateKeys ts i c = let
    val (iIns, iRes) = getInsResI i c
    val tsIns = TS.instantiate ts iIns in
    (tsIns, iRes) end

  fun getFunTyschRes (vs, TY.FUNTY (t1, t2)) _ = TS.reg (vs, t2)
    | getFunTyschRes (vs, TY.ASSTY t) insmap =
    (getFunTyschRes (valOf (I.find (insmap, t))) insmap
    handle Option => raise TY.WrongTypeForm "getFunTyschRes find no entry in insmap")
    | getFunTyschRes _ _ =
    raise TY.WrongTypeForm "WRONG TYPE FORM AT getFunTyschRes"

  fun infScon (INT_SCON _) =  ISB.intTysch
    | infScon (REAL_SCON _) = ISB.realTysch
    | infScon (WORD_SCON _) = ISB.wordTysch
    | infScon (CHAR_SCON _) = ISB.charTysch
    | infScon (STR_SCON _)  = ISB.strTysch

  fun infAtexp c atexp = let val (ts, i, syn) = case atexp of 
      (SCON_ATEXP scon) => (infScon scon, I.empty, IST.SCON_ATEXP scon)
    | (LVID_ATEXP lvid) => (
    (#1 (Option.valOf (C.getValstr c lvid)), I.empty, IST.LVID_ATEXP lvid)
    handle Option => (
      TIO.println ((LongValueIdentifier.toString lvid) ^ " not defined");
      raise StaticInferenceFail "unknown long value identifier"))
    | (RCD_ATEXP exprow) => infExprow c exprow
    | (LET_ATEXP (dec, exp)) => let
    (* TODO: type name escape avoidence *)
    val (eDec, iDec, sDec) = infDec c dec
    val cExp = C.envAugment c eDec
    val (tsExp, iExp, sExp) = infExp cExp exp
    val insmap = iUnify c iExp iDec in
    (tsExp, insmap, IST.LET_ATEXP (sDec, sExp)) end
    | (EXP_ATEXP exp) => let 
      val (a, b, sExp) = infExp c exp in
      (a, b, IST.EXP_ATEXP sExp) end in 
    (ts, i, (syn, C.groundTysch c ts)) end
(*
  fun infAtexp c (SCON_ATEXP scon) = (infScon scon, I.empty, IST.SCON_ATEXP scon)
    | infAtexp c (LVID_ATEXP lvid) = (
    (#1 (Option.valOf (C.getValstr c lvid)), I.empty, IST.LVID_ATEXP lvid)
    handle Option => raise StaticInferenceFail "unknown long value identifier")
    | infAtexp c (RCD_ATEXP exprow) = infExprow c exprow
    | infAtexp c (LET_ATEXP (dec, exp)) = let
    (* TODO: type name escape avoidence *)
    val (eDec, iDec, sDec) = infDec c dec
    val cExp = C.envAugment c eDec
    val (tsExp, iExp, sExp) = infExp cExp exp
    val insmap = iUnify c iExp iDec in
    (tsExp, insmap, IST.LET_ATEXP (sDec, sExp)) end
    | infAtexp c (EXP_ATEXP exp) = let 
      val (a, b, sExp) = infExp c exp in
      (a, b, IST.EXP_ATEXP sExp) end *)

  and infExprow c exprow = let val (ts, i, syn) =
    List.foldl (fn ((lab, exp), (ts, i, syn)) => let
      val (tsExp, iExp, sExp) = infExp c exp
      val ts = TS.insertRowTysch ts lab tsExp
      val insmap = iUnify c iExp i 
      val syn = (lab, sExp) :: syn in (ts, insmap, syn) end)
      (ISB.unitTysch, I.empty, []) exprow in 
    (ts, i, IST.RCD_ATEXP (List.rev syn)) end

  and infExp c exp = let val (ts, i, syn) = case exp of
      (AT_EXP atexp) => let 
      val (a, b, sAtexp) = infAtexp c atexp in
      (a, b, IST.AT_EXP sAtexp) end

    | (APP_EXP (exp, atexp)) => let
      val (tsExp, iExp, sExp) = infExp c exp
      val (tsAtexp, iAtexp, sAtexp) = infAtexp c atexp
      val (tsExp', iExp') = case tsExp of
        (_, TY.ASSTY a) => let
          val newAt = newAssty (SOME a)
          val tsExp' = TS.getFunTysch tsAtexp (VS.empty, TY.ASSTY newAt)
          val iExp' = iUnify c iExp (I.fromListPair [(newAt, TS.wild)]) in
          (tsExp', iExp') end
      | tsExp => let
          val newAt = newAssty NONE
          val tsExp' = TS.getFunTysch tsAtexp (VS.empty, TY.ASSTY newAt)
          val iExp' = iUnify c iExp (I.fromListPair [(newAt, TS.wild)]) in
          (tsExp', iExp') end

    val (tsUni, iUni) = tsUnify c tsExp tsExp'
    (*TODO: no longer necessary*)
    val ts = getFunTyschRes tsUni iUni
    val insmap = iUnify c (iUnify c iExp' iAtexp) iUni in
    (ts, insmap, IST.APP_EXP (sExp, sAtexp)) end

    | (INF_EXP (exp1, vid, exp2)) => raise StaticInferenceFail "INFIX"

    | (TY_EXP (exp, ty)) => let
    val (tsExp, iExp, (sExp, _)) = infExp c exp
    val tsTy = infTy c ty
    val (tsUni, iUni) = tsUnify c tsExp tsTy
    val insmap = iUnify c iExp iUni in
    (tsUni, insmap, sExp) end

    (*| (HAND_EXP (exp, match)) => let*)
    (*val (tsExp, iExp, sExp) = infExp c exp*)
    (*val (tsMatch, iMatch, sMatch) = infMatch c match*)
    (*val tsExp' = getFunTyschRes tsMatch iMatch*)
    (*val (tsUni, iUni) = tsUnify c tsExp tsExp'*)
    (*val insmap' = iUnify c (iUnify c iMatch iUni) iExp in*)
    (*(tsUni, insmap',  IST.HAND_EXP (sExp, sMatch)) end*)

    | (RAS_EXP exp) => let
    val (tsExp, iExp, sExp) = infExp c exp
    val _ = tsUnify c tsExp ISB.exnTysch in
    (TS.wild, iExp, IST.RAS_EXP sExp) end

    | (FN_EXP match) => let 
      val (tsMatch, iMatch, sMatch) = infMatch c match in
      (tsMatch, iMatch, IST.FN_EXP sMatch) end in
    TIO.println (CST.expToString exp);
    TIO.println (TS.toString ts);
    TIO.println (I.toString i);
    (ts, i, (syn, C.groundTysch c ts)) end

  and infMatch   c match =  let 
    val (ts, i ,revsyn) = 
    List.foldl (fn (mrule, (ts, i, syn)) => let
      val (tsMrule, iMrule, sMrule) = infMrule c mrule
      val (tsUni, iUni) = tsUnify c tsMrule ts
      val insmap = iUnify c (iUnify c iMrule i) iUni in
      (tsUni, insmap, sMrule :: syn) end) 
      (TS.wild, I.empty, []) match in 
    (ts, i, (List.rev revsyn, matchComp match c)) end

  and infMrule   c (pat, exp) = let
    val (vePat, tsPat, iPat, sPat) = infPat c pat
    val newAt = VE.getAsstyset vePat
    val (tsExp, iExp, sExp) = infExp (C.valenvAugment c vePat) exp
    val insmap = iUnify c iExp iPat
    val tsFun = TS.getFunTysch tsPat tsExp in
    (tsFun, insmap, (sPat, sExp)) end

  and infDec c dec = let val (e, i, s) = case dec of

     (VAL_DEC (tvseq, valbd)) => let
    val c' = C.addTyvarseq c tvseq
    val (veValbd, iValbd, sValbd) = infValbd c' valbd
    val closVeValbd = C.closValenv c veValbd
    val env = E.fromValenv closVeValbd in
    (env, iValbd, IST.VAL_DEC sValbd) end

    | (TYP_DEC typbd) => let
    val ve = infTypbd c typbd
    val env = E.fromTyenv ve in
    (env, I.empty, IST.SEQ_DEC []) end

    (* TODO: equality and check *)
    | (DAT_DEC datbd) => let
    val (teIntro, tIntro) = introDatbd c datbd
    val c' = C.tyenvAugment c teIntro
    val c'' = C.tynameenvAugment c' tIntro
    val (ve, te, sDatbd) = infDatbd c'' datbd in
    (E.ENV (SE.empty, te, ve), I.empty, IST.DAT_DEC sDatbd) end

    | (EXC_DEC exbd) => let
    val (ve, sExbd) = infExbd c exbd in
    (E.fromValenv ve, I.empty, IST.EXC_DEC sExbd) end

    | (LOC_DEC  (dec1, dec2)) => let
    val (eDec1, ieDec1, sDec1) = infDec c dec1
    val c' = C.envAugment c eDec1
    val (eDec2, ieDec2, sDec2) = infDec c' dec2
    val ie = iUnify c ieDec1 ieDec2 in
    (eDec2, ie, IST.LOC_DEC (sDec1, sDec2)) end

    | (SEQ_DEC  (dec1, dec2)) => let
    val (eDec1, iDec1, sDec1) = infDec c dec1
    val (eDec2, iDec2, sDec2) = infDec (C.envAugment c eDec1) dec2
    val i = iUnify c iDec1 iDec2
    val e = E.modify eDec1 eDec2 in (e, i, IST.decCombine sDec1 sDec2) end

    |  _ => raise StaticInferenceFail "NOT IMPLEMENTED" in 
    (e, i, s) end

    (* no longer supported *)
    (*| infDec     c (REP_DEC  (tc, ltc)) = let*)
    (*val ts = Option.valOf (C.getTystr c ltc)*)
    (*val te = TE.fromListPair [(tc, ts)] in*)
    (*(E.fromTyenv te, I.empty) end*)

    (*| infDec     c (ABS_DEC (datbd, dec)) = let*)
    (*val (envDatbd, ieDatbd) = infDec c (DAT_DEC datbd)*)
    (*val c' = C.envAugment c envDatbd*)
    (*val (envDec, ieDec) = infDec c dec*)
    (*val ie = iUnify c ieDatbd ieDec in*)
    (*(envDec, ie) end*)

    (*| infDec     c (OPEN_DEC lvidseq) = let*)
    (*fun aux (lvid :: lvidseq) =*)
      (*E.modify (C.getEnv c lvid) (aux lvidseq)*)
      (*| aux [] = E.empty in*)
    (*(aux lvidseq, I.empty) end*)


  and infValbd   c (NRE_VALBIND vrow) = infVrow c vrow false
    | infValbd   c (REC_VALBIND vrow) = infVrow c vrow true

  and infVrow c vrow recu = if recu then let
      val vetsisynPats = List.map (fn (pat, exp) => infPat c pat ) vrow
      val (vePat, iPat) = List.foldl (fn ((vePat, _, iPat, _), (ve, i)) =>
        (VE.modify ve vePat, iUnify c i iPat)) (VE.empty, I.empty) vetsisynPats
      val recc = C.valenvAugment c vePat
      val tsisynExps = List.map (fn (pat, exp) => infExp recc exp)  vrow
      val (iUnis, sVrow) = ListPair.unzip (
        List.map (fn ((_,tsPat,iPat,sPat),(tsExp,iExp,sExp)) => let
        val (ts, i1) = tsUnify c tsPat tsExp
        val i = iUnify c i1 (iUnify c iPat iExp) in (i, (sPat, sExp)) end)
        (ListPair.zip (vetsisynPats, tsisynExps)))
      val (ve, i, revinsvrow) = List.foldl (
      fn ((((vePat,_,_,sPat), iUni), (_,_,sExp)), (ve, i, vrow)) => let
        val (vePatIns, iUniRes) = veInstantiateKeys vePat iUni c in
        (VE.modify ve vePatIns, iUnify c i iUniRes,
         (patsynInstantiateKeys sPat iUni c, 
          expsynInstantiateKeys sExp iUni c):: vrow) end)
        (VE.empty, I.empty, []) 
        (ListPair.zip (ListPair.zip (vetsisynPats, iUnis), tsisynExps)) 
      val vrow' = List.rev revinsvrow in 
      (ve, i, IST.REC_VALBIND (vrow', vrowComp vrow c)) end
    else let
      val (ve, i, revsyn) = List.foldl (fn ((pat, exp), (ve, i, syn)) => let
        val (vePat, tsPat, iPat, sPat) = infPat c pat
        val (tsExp, iExp, sExp) = infExp c exp
        val (tsUni, i1) = tsUnify c tsPat tsExp
        val iUni = iUnify c (iUnify c iPat iExp) i1
        val (vePatIns, iRes) = veInstantiateKeys vePat iUni c in
        (VE.modify ve vePatIns, iUnify c i iRes, 
         (patsynInstantiateKeys sPat iUni c, 
          expsynInstantiateKeys sExp iUni c) :: syn) end)
        (VE.empty, I.empty, []) vrow 
        val vrow' = List.rev revsyn in
      (ve, i, IST.NRE_VALBIND (vrow', vrowComp vrow c)) end

  and infTypbd   c ((tvs, tc, ty) :: typbd) = let
    val c' = C.addTyvarseq c tvs
    val tsTy = infTy c' ty
    val args = List.map (fn tv => Option.valOf (C.getVarty c' tv)) tvs
    val tyfcn = (args, #2 tsTy)
    val tyenv = TE.fromListPair [(tc, (tyfcn, VE.empty))]
    val teTypbd = infTypbd c typbd in
    TE.modify tyenv teTypbd end
    | infTypbd c [] = TE.empty

  and introDatbd c [(tvs, tc, conbd)] = let
    val tyname = (([], tc), length tvs, false)
    val tyfcn = TF.fromTyname tyname
    val tyenv = TE.fromListPair [(tc, (tyfcn, VE.empty))]
    val tynameset = T.fromListPair [(([], tc), tyname)] in
    (tyenv, tynameset) end

    | introDatbd c (dat :: datbd) = let
    val (tyenv, tynameset) = introDatbd c [dat]
    val (tyenv', tynameset') = introDatbd c datbd
    val tyenv'' = TE.modify tyenv tyenv'
    val tynameset'' = T.modify tynameset tynameset' in
    (tyenv'', tynameset'') end

    | introDatbd c [] = (TE.empty, T.empty)

  and infDatbd c (datbd) = let

    fun aux (tvs, tc, conbd) = let
      val c' = C.addTyvarseq c tvs
      val vts = List.map (fn tv => TY.VARTY (Option.valOf (C.getVarty c' tv))) tvs
      val tns = Option.valOf (C.getTyname c' ([], tc))
      val ts = (VartySet.empty, TY.CONTY (vts, tns)) in
      infConbd c' ts conbd end

    val (veDatbd, teDatbd, revsDatbd) = List.foldl (fn 
      (dat as (tvs, tc, conbd),(ve, te, syn)) => let
      val (tfDat, _) = Option.valOf (C.getTystr c ([], tc))
      val (veDatRaw, sDat) = aux dat
      val veDat = C.closValenv c veDatRaw
      val teDat = TE.fromListPair [(tc, (tfDat, veDat))]
      val ve = VE.modify veDat ve
      val te = TE.modify teDat te in (ve, te, sDat :: syn) end)
      (VE.empty, TE.empty, []) datbd in 
      (veDatbd, teDatbd, List.rev revsDatbd) end

  and infConbd c ts conbd = let
    val (veConbd, revsConbd) = List.foldl (fn ((vid, tyop), (ve, syn)) => 
      if isSome tyop then let
        val ty = valOf tyop
        val tsTy = infTy c ty
        val tsCon = TS.getFunTysch tsTy ts
        val veCon = VE.fromListPair [(vid, (tsCon, CON))] in
        (VE.modify ve veCon, (vid, true) :: syn) end
      else let
        val veCon = VE.fromListPair [(vid, (ts, CON))] in
        (VE.modify ve veCon, (vid, false) :: syn) end) (VE.empty, []) conbd in
    (veConbd, List.rev revsConbd) end

  and infExbd c exbd = let
    fun aux (VID_EXBIND_ELE (vid, tyOp)) = let
      val veBase = VE.fromListPair [(vid, (ISB.exnTysch, CON))] in
      if Option.isSome tyOp then let
        val ty = Option.valOf tyOp
        val tsTy = infTy c ty
        val tsExn = TS.getFunTysch tsTy ISB.exnTysch
        val ve = VE.fromListPair [(vid, (tsExn, CON))]in
        (ve, (vid, true)) end
      else (veBase, (vid, false))
      end
      | aux (REP_EXBIND_ELE (vid, lvid)) = raise Match 
    
    val (veExbd, revsExbd) = List.foldl (fn (ex, (ve, syn)) => let 
        val (veEx, sEx) = aux ex in 
        (VE.modify ve veEx, sEx :: syn) end)
      (VE.empty, []) exbd in
      (veExbd, List.rev revsExbd) end

  and infAtpat c atpat = let val (ve, ts, i, syn) = case atpat of
       WILD_ATPAT => (VE.empty, TS.wild, I.empty, IST.WILD_ATPAT)
    |  (SCON_ATPAT scon) =>
    (VE.empty, infScon scon, I.empty, IST.SCON_ATPAT scon)
    |  (LVID_ATPAT (lvid as (pre, vid))) =>
    if List.null pre then let
      val vsVidOp  = C.getValstr c lvid
      val (tsVidOp, isVid) = Option.getOpt (vsVidOp, (ISB.unitTysch, VAL)) in
      if isVid = VAL then let
        val newAt = newAssty NONE
        val ts = (VS.empty, TY.ASSTY newAt)
        val ve = VE.fromListPair [(vid, (ts ,VAL))] in
        (ve, ts, I.fromListPair [(newAt, TS.wild)], IST.LVID_ATPAT lvid) end
      else 
        (VE.empty, tsVidOp, I.empty, IST.LVID_ATPAT lvid) end
    else raise StaticInferenceFail "no structure supported"
    |  (RCD_ATPAT patrow) => infPatrow c patrow
    |  (PAT_ATPAT pat) => let
      val (a, b, c, sPat) = infPat c pat in
      (a, b, c, IST.PAT_ATPAT sPat) end in 
    (ve, ts, i, (syn, C.groundTysch c ts)) end

(*
  and infAtpat   c WILD_ATPAT = (VE.empty, TS.wild, I.empty, IST.WILD_ATPAT)
    | infAtpat   c (SCON_ATPAT scon) = 
    (VE.empty, infScon scon, I.empty, IST.SCON_ATPAT scon)
    | infAtpat   c (LVID_ATPAT (lvid as (pre, vid))) =
    if List.null pre then let
      val vsVidOp  = C.getValstr c lvid
      val (tsVidOp, isVid) = Option.getOpt (vsVidOp, (ISB.unitTysch, VAL)) in
      if isVid = VAL then let
        val newAt = newAssty NONE
        val ts = (VS.empty, TY.ASSTY newAt)
        val ve = VE.fromListPair [(vid, (ts ,VAL))] in
        (ve, ts, I.fromListPair [(newAt, TS.wild)], IST.LVID_ATPAT lvid) end
      else 
        (VE.empty, tsVidOp, I.empty, IST.LVID_ATPAT lvid) end
    else raise StaticInferenceFail "no structure supported" 
    | infAtpat   c (RCD_ATPAT patrow) = infPatrow c patrow
    | infAtpat   c (PAT_ATPAT pat) = let
      val (a, b, c, sPat) = infPat c pat in
      (a, b, c, IST.PAT_ATPAT sPat) end *)

  and infPatrow c (patrow, isWild) = let
    val (vePatrow, tsPatrow, iPatrow, revsPatrow) = List.foldl 
      (fn ((lab, pat), (ve, ts, i, syn)) => let
        val (vePat, tsPat, iPat, sPat) = infPat c pat
        val ve = VE.modify ve vePat
        val ts = TS.insertRowTysch ts lab tsPat
        val i = iUnify c iPat i in
        (ve, ts, i, (lab, sPat) :: syn) end)
      (VE.empty, if isWild then ISB.wildUnitTysch else ISB.unitTysch,
       I.empty, []) patrow in 
     (vePatrow, tsPatrow, iPatrow, IST.RCD_ATPAT (List.rev revsPatrow)) end

  and infPat c pat = let val (ve, ts, i, syn) = (
    case pat of
      (AT_PAT atpat) => let
      val (a, b, c, sAtpat) = infAtpat c atpat in
      (a, b, c, IST.AT_PAT sAtpat) end
    | (CON_PAT (lvid, atpat)) => let
    val tsOp = C.getValstr c lvid in
    if Option.isSome tsOp then let
      val (tsCon, ieCon) = Option.valOf tsOp in
      if not (ieCon = VAL) then let
        val (veAtpat, tsAtpat, iAtpat, sAtpat) = infAtpat c atpat
        val tsFunAtpat = TS.getFunTysch tsAtpat TS.wild
        val (tsFunUni, iFunUni) = tsUnify c tsCon tsFunAtpat
        val insmap = iUnify c iAtpat iFunUni
        val ts = getFunTyschRes tsFunUni insmap in
        (veAtpat, ts, insmap, IST.CON_PAT ((lvid, tsCon), sAtpat)) end
      else raise StaticInferenceFail "CONSTRUCTOR IS A VAL" end
    else raise StaticInferenceFail "LVID PAT NOT IN ENV" end

    | (TY_PAT (pat, ty)) => let
    val (vePat, tsPat, iPat, (sPat, _)) = infPat c pat
    val tsTy = infTy c ty
    val (ts, iUni) = tsUnify c tsPat tsTy
    val insmap = iUnify c iUni iPat in
    (vePat, ts, insmap, sPat) end

    | (LAY_PAT (vid, tyOp, pat)) => let
    val tsVidOp = C.getValstr c ([], vid)
    val (_, isVid) = Option.valOf tsVidOp
      handle Option => (TS.wild ,VAL)
    val tsTy = infTy c (Option.valOf tyOp)
      handle Option => TS.wild
    val (vePat, tsPat, iPat, sPat) = infPat c pat
    val someVidInVe = VE.find (vePat, vid)
    val (tsUni, iUni) = tsUnify c tsTy tsPat
    val ve = VE.insert (vePat, vid, (tsUni, VAL)) in
    if isVid = VAL andalso not (Option.isSome someVidInVe) then
      (ve, tsUni, iUni, IST.LAY_PAT (vid, sPat)) else
      raise StaticInferenceFail "LAY PAT GET VID AS NOT VAL OR IN VEPAT" end) in
    (ve, ts, i, (syn, C.groundTysch c ts)) end

  and infTy c ty = let val res = (case ty of
    (VAR_TY tyvar) => let
    val isTyvar = C.isTyvar c tyvar in
    if isTyvar then let
      val varty = Option.valOf (C.getVarty c tyvar) in
      (VS.empty, TY.VARTY varty) end else
      raise StaticInferenceFail "TYVAR NOT EXPLICITLY DEFINED" end

    | (RCD_TY tyrow) => infTyrow c tyrow
    | (CON_TY (tyseq, ltycon)) => let
    val tsseq = List.map (fn ty => infTy c ty) tyseq
    val tsLtyconOp = C.getTystr c ltycon in
    if Option.isSome tsLtyconOp then let
      val tynameOp = C.getTyname c ltycon in
      if Option.isSome tynameOp then let
        val tyname = Option.valOf tynameOp
        val ts = TF.appTyname tsseq tyname in ts end
      else raise StaticInferenceFail "INVALID TYNAME" end
    else raise StaticInferenceFail "INVALID TYCON" end 

    | (FUN_TY (argty, resty)) => let
    val tsArg = infTy c argty
    val tsRes = infTy c resty
    val tsFun = TS.getFunTysch tsArg tsRes in tsFun end) in
    res end

  and infTyrow   c ((lab, ty) :: tyrow) = let
    val tsTy = infTy c ty
    val tsTyrow = infTyrow c tyrow
    val ts = TS.insertRowTysch tsTyrow lab tsTy in ts end
    | infTyrow c [] = ISB.unitTysch

  (* ASSERT that insmap empty *)
  fun inference prog = let 
    val (env, _, ist) = infDec ISB.context prog in 
    (env, IntermediateSyntaxTree.fillProg ist) end

end
