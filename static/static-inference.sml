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

  fun infScon (INT_SCON _) =  ISB.intTysch
    | infScon (REAL_SCON _) = ISB.realTysch
    | infScon (WORD_SCON _) = ISB.wordTysch
    | infScon (CHAR_SCON _) = ISB.charTysch
    | infScon (STR_SCON _)  = ISB.strTysch

  fun infAtexp c (SCON_ATEXP scon) = (infScon scon, I.empty)
    | infAtexp c (LVID_ATEXP lvid) = (
    (#1 (Option.valOf (C.getValstr c lvid)), I.empty)
    handle Option => raise StaticInferenceFail "unknown long value identifier")
    | infAtexp c (RCD_ATEXP exprow) = infExprow c exprow
    | infAtexp c (LET_ATEXP (dec, exp)) = let
    (* TODO: type name escape avoidence *)
    val (eDec, iDec) = infDec c dec
    val cExp = C.envAugment c eDec
    val (tsExp, iExp) = infExp cExp exp
    val insmap = I.unify iExp iDec in
    (tsExp, insmap) end
    | infAtexp c (EXP_ATEXP exp) = infExp c exp

  and infExprow c ((lab, exp) :: exprow) = let
    val (tsExp, iExp) = infExp c exp
    val (tsExprow, iExprow) = infExprow c exprow
    val ts = TS.insertRowTysch tsExprow lab tsExp
    val insmap = I.unify iExp iExprow in
    (ts, insmap) end
    | infExprow c [] = (ISB.unitTysch, I.empty)

  and infExp     c (AT_EXP atexp) = infAtexp c atexp

    | infExp     c (APP_EXP (exp, atexp)) = let
    val (tsExp, iExp) = infExp c exp
    val (tsAtexp, iAtexp) = infAtexp c atexp
    val tsAtexp' = TS.getFunTysch tsAtexp TS.wild
    val (tsUnify, iUnify) = TS.unify tsExp tsAtexp'
    val ts = TS.getFunTyschRes tsUnify
    val insmap = I.unify (I.unify iExp iAtexp) iUnify in
    (ts, insmap) end

    | infExp     c (INF_EXP (exp1, vid, exp2)) = raise StaticInferenceFail "INFIX"

    | infExp     c (TY_EXP (exp, ty)) = let
    val (tsExp, iExp) = infExp c exp
    val tsTy = infTy c ty
    val (tsUnify, iUnify) = TS.unify tsExp tsTy
    val insmap = I.unify iExp iUnify in
    (tsUnify, insmap) end

    | infExp     c (HAND_EXP (exp, match)) = let
    val (tsExp, iExp) = infExp c exp
    val (tsMatch, iMatch) = infMatch c match
    val tsExp' = TS.getFunTyschRes tsMatch
    val (tsUnify, iUnify) = TS.unify tsExp tsExp'
    val insmap' = I.unify (I.unify iMatch iUnify) iExp in
    (tsUnify, insmap') end

    | infExp     c (RAS_EXP exp) = let
    val (tsExp, iExp) = infExp c exp
    val _ = TS.unify tsExp ISB.exnTysch in
    (TS.wild, iExp) end

    | infExp     c (FN_EXP match) = infMatch c match

  and infMatch   c [mrule] = infMrule c mrule
    | infMatch   c (mrule :: match) = let
    val (tsMrule, iMrule) = infMrule c mrule
    val (tsMatch, iMatch) = infMatch c match
    val (tsUnify, iUnify) = TS.unify tsMrule tsMatch
    val insmap = I.unify (I.unify iMrule iMatch) iUnify in
    (tsUnify, insmap) end
    | infMatch   _ _ = raise StaticInferenceFail "INVALID SYNTAX"

  and infMrule   c (pat, exp) = let
    val (vePat, tsPat, iPat) = infPat c pat
    val newAt = VE.getAsstyset vePat
    val (tsExp, iExp) = infExp (C.valenvAugment c vePat) exp
    val insmap = I.unify iExp iPat
    val tsFun = TS.getFunTysch tsPat tsExp
    val insseq = I.listItemsi (I.intersectKeyset insmap newAt)
    val tsFun' = TS.instantiate tsFun insseq
    val insmap' = I.removeKeyset insmap newAt in
    (tsFun', insmap') end

  and infDec     c (VAL_DEC (tvseq, valbd)) = let
    val c' = C.addTyvarseq c tvseq
    val (veValbd, iValbd) = infValbd c' valbd
    val closVeValbd = C.closValenv c veValbd
    val env = E.fromValenv closVeValbd in
    (env, iValbd) end

    | infDec     c (TYP_DEC typbd) = let
    val ve = infTypbd c typbd
    val env = E.fromTyenv ve in
    (env, I.empty) end

    (* TODO: equality and check *)
    | infDec     c (DAT_DEC datbd) = let
    val (teIntro, tIntro) = introDatbd c datbd
    val c' = C.tyenvAugment c teIntro
    val c'' = C.tynameenvAugment c' tIntro
    val (ve, te) = infDatbd c'' datbd in
    (E.ENV (SE.empty, te, ve), I.empty) end

    | infDec     c (REP_DEC  (tc, ltc)) = let
    val ts = Option.valOf (C.getTystr c ltc)
    val te = TE.fromListPair [(tc, ts)] in
    (E.fromTyenv te, I.empty) end

    | infDec     c (ABS_DEC (datbd, dec)) = let
    val (envDatbd, ieDatbd) = infDec c (DAT_DEC datbd)
    val c' = C.envAugment c envDatbd
    val (envDec, ieDec) = infDec c dec
    val ie = I.unify ieDatbd ieDec in
    (envDec, ie) end

    | infDec     c (EXC_DEC exbd) = let
    val ve = infExbd c exbd in
    (E.fromValenv ve, I.empty) end

    | infDec     c (LOC_DEC  (dec1, dec2)) = let
    val (eDec1, ieDec1) = infDec c dec1
    val c' = C.envAugment c eDec1
    val (eDec2, ieDec2) = infDec c' dec2
    val ie = I.unify ieDec1 ieDec2 in
    (eDec2, ie) end

    | infDec     c (OPEN_DEC lvidseq) = let
    fun aux (lvid :: lvidseq) =
      E.modify (C.getEnv c lvid) (aux lvidseq)
      | aux [] = E.empty in
    (aux lvidseq, I.empty) end

    | infDec     c (SEQ_DEC  (dec1, dec2)) = let
    val (eDec1, iDec1) = infDec c dec1
    val (eDec2, iDec2) = infDec (C.envAugment c eDec1) dec2
    val () = print ((E.toString eDec1) ^ " --- infDec seq 1\n")
    val () = print ((E.toString eDec2) ^ " --- infDec seq 2\n")
    val i = I.unify iDec1 iDec2
    val e = E.modify eDec1 eDec2 in (e, i) end

    | infDec     c (INF_DEC  _) = raise StaticInferenceFail "INFDEC NOT IMPLEMENTED"
    | infDec     c (INFR_DEC _) = raise StaticInferenceFail "INFDEC NOT IMPLEMENTED"
    | infDec     c (NOF_DEC  _) = raise StaticInferenceFail "INFDEC NOT IMPLEMENTED"

  and infValbd   c (NRE_VALBIND vrow) = infVrow c vrow false
    | infValbd   c (REC_VALBIND vrow) = infVrow c vrow true

  and introVrow c [(pat, exp)] = let
    val (vePat, tsPat, iPat) = infPat c pat in
    (vePat, iPat) end
    | introVrow c ((pat, exp) :: valbd) = let
    val (vePat, tsPat, iPat) = infPat c pat
    val (veValbd, iValbd) = introVrow c valbd
    val ve = VE.modify vePat veValbd
    val insmap = I.unify iPat iValbd in
    (ve, insmap) end
    | introVrow _ _ = raise StaticInferenceFail "EMPTY VROW REC"

  and infVrow c ((pat, exp) :: valbd) recu = if recu then let
      val (veIntro, iIntro) = introVrow c ((pat, exp) :: valbd)
      val c' = C.valenvAugment c veIntro
      val (ve, insmap) = infVrow c' ((pat, exp) :: valbd) false
      val insmap' = I.unify iIntro insmap in
      (print ((I.toString insmap') ^ "\n")); (ve, insmap') end

    else let
      val (vePat, tsPat, iPat) = infPat c pat
      val () = print ((VE.toString vePat) ^ " --- infVrow vePat \n")
      val () = print ((TS.toString tsPat) ^ " --- infVrow tsPat \n")
      val () = print ((I.toString iPat) ^ " --- infVrow iPat \n")

      val newAsstyset = VE.getAsstyset vePat
      val (tsExp, iExp) = infExp c exp
      val () = print ((TS.toString tsPat) ^ " --- infVrow tsExp \n")
      val () = print ((I.toString iExp) ^ " --- infVrow iExp \n")

      val (tsUnify, iUnify) = TS.unify tsPat tsExp
      val insmap = I.unify (I.unify iPat iExp) iUnify
      val veI = I.intersectKeyset insmap newAsstyset
      val vePat' = VE.instantiate vePat (I.listItemsi veI)
      val insmap' = I.removeKeyset insmap newAsstyset
      val (veValbd, iValbd) = infVrow c valbd false
      val ve = VE.modify vePat' veValbd
      val insmap'' = I.unify insmap' iValbd in
      (print ((I.toString insmap'') ^ " --- infVrow\n")); (ve, insmap'') end
    | infVrow c [] _ = (VE.empty, I.empty)

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

  and infDatbd c ((dat as (tvs, tc, conbd)) :: datbd) = let

    fun aux (tvs, tc, conbd) = let
      val c' = C.addTyvarseq c tvs
      val vts = List.map (fn tv => TY.VARTY (Option.valOf (C.getVarty c' tv))) tvs
      val tns = Option.valOf (C.getTyname c' ([], tc))
      val ts = (VartySet.empty, TY.CONTY (vts, tns)) in
      infConbd c' ts conbd end
    val (tfDat, _) = Option.valOf (C.getTystr c ([], tc))
    val veDat = aux dat
    val teDat = TE.fromListPair [(tc, (tfDat, veDat))]
    val (veDatbd, teDatbd) = infDatbd c datbd
    val ve = VE.modify veDat veDatbd
    val te = TE.modify teDat teDatbd in (ve, te) end
    | infDatbd c [] = (VE.empty, TE.empty)

  and infConbd c ts ((vid, NONE) :: conbd) = let
    val ve = VE.fromListPair [(vid, (ts, CON))]
    val veConbd = infConbd c ts conbd in
    VE.modify ve veConbd end
    | infConbd c ts ((vid, SOME ty) :: conbd) = let
    val tsTy = infTy c ty
    val tsCon = TS.getFunTysch tsTy ts
    val ve = VE.fromListPair [(vid, (tsCon, CON))]
    val veConbd = infConbd c ts conbd in
    VE.modify ve veConbd end
    | infConbd c ts [] = VE.empty

  and infExbd c (ex :: exbd) = let
    fun aux c (VID_EXBIND_ELE (vid, tyOp)) = let
      val veBase = VE.fromListPair [(vid, (ISB.exnTysch, CON))] in
      if Option.isSome tyOp then let
        val ty = Option.valOf tyOp
        val tsTy = infTy c ty
        val tsExn = TS.getFunTysch tsTy ISB.exnTysch
        val ve = VE.fromListPair [(vid, (tsExn, CON))]in
        ve end
      else veBase
      end
      | aux c (REP_EXBIND_ELE (vid, lvid)) = let
      val vsLvid = Option.valOf (C.getValstr c lvid)
      val ve = VE.fromListPair [(vid, vsLvid)] in
      ve end in
    VE.modify (aux c ex) (infExbd c exbd) end

    | infExbd c [] = VE.empty

  and infAtpat   c WILD_ATPAT = (VE.empty, TS.wild, I.empty)
    | infAtpat   c (SCON_ATPAT scon) = (VE.empty, infScon scon, I.empty)
    | infAtpat   c (LVID_ATPAT (lvid as (pre, vid))) = let
    val () = print ((LongValueIdentifier.toString lvid) ^ " --- infAtpat \n")
    fun aux () = let
      val vsOp = C.getValstr c lvid in
      if Option.isSome vsOp then let
      val (ts, is) = Option.valOf vsOp in
      if not (is = VAL) then
        (VE.empty, ts, I.empty) else
        raise StaticInferenceFail "LVID VAL IN PATTERN" end
      else raise StaticInferenceFail "logically impossible return" end

    val asstyset = C.getAsstyset c in
    if List.null pre then let
      val vid = #2 lvid
      val vsVidOp  = C.getValstr c lvid
      val (tsVidOp, isVid) = Option.getOpt (vsVidOp, (ISB.unitTysch, VAL)) in
      if not (Option.isSome vsVidOp) orelse isVid = VAL then let
        val newAssty = AS.getNewAssty asstyset
        val ts = (VS.empty, TY.ASSTY newAssty)
        val ve = VE.fromListPair [(vid, (ts ,VAL))]
        val i = I.fromListPair [(newAssty, TS.wild)] in
        (ve, ts, i) end
      else aux () end
    else aux () end
    | infAtpat   c (RCD_ATPAT patrow) = infPatrow c patrow
    | infAtpat   c (PAT_ATPAT pat) = infPat c pat

  (* TODO: implement wild record *)
  and infPatrow  c ((lab, pat) :: patrow, isWild) = let
    val (vePat, tsPat, iPat) = infPat c pat
    val (vePatrow, tsPatrow, iPatrow) = infPatrow c (patrow, isWild)
    val ve = VE.unionWith (fn _ =>
      raise StaticInferenceFail "DUP PAT VID") (vePat, vePatrow)
    val ts = TS.insertRowTysch tsPat lab tsPatrow
    val insmap = I.unify iPat iPatrow in
    (ve, ts, insmap) end
    | infPatrow _ ([], isWild) = (VE.empty, ISB.unitTysch, I.empty)

  and infPat     c (AT_PAT atpat) = infAtpat c atpat
    | infPat     c (CON_PAT (lvid, atpat)) = let
    val tsOp = C.getValstr c lvid in
    if Option.isSome tsOp then let
      val (tsCon, ieCon) = Option.valOf tsOp in
      if not (ieCon = VAL) then let
        val (veAtpat, tsAtpat, iAtpat) = infAtpat c atpat
        val tsFunAtpat = TS.getFunTysch tsAtpat TS.wild
        val (tsFunUnify, iFunUnify) = TS.unify tsCon tsAtpat
        val insmap = I.unify iAtpat iFunUnify
        val ts = TS.getFunTyschRes tsFunUnify in
        (veAtpat, ts, insmap) end
      else raise StaticInferenceFail "CONSTRUCTOR IS A VAL" end
    else raise StaticInferenceFail "LVID PAT NOT IN ENV" end

    | infPat     c (INF_PAT _) =
    raise StaticInferenceFail "INFIX PAT NOT SUPPORTED"
    | infPat     c (TY_PAT (pat, ty)) = let
    val (vePat, tsPat, iPat) = infPat c pat
    val tsTy = infTy c ty
    val (ts, iUnify) = TS.unify tsPat tsTy
    val insmap = I.unify iUnify iPat in
    (vePat, ts, insmap) end

    | infPat     c (LAY_PAT (vid, tyOp, pat)) = let
    val tsVidOp = C.getValstr c ([], vid)
    val (tsVid, isVid) = Option.valOf tsVidOp
      handle Option => (TS.wild ,VAL)
    val asstyset = C.getAsstyset c
    val tsTy = infTy c (Option.valOf tyOp)
      handle Option => TS.wild
    val (vePat, tsPat, iPat) = infPat c pat
    val someVidInVe = VE.find (vePat, vid)
    val (tsUnify, iUnify) = TS.unify tsTy tsPat
    val ve = VE.insert (vePat, vid, (tsUnify, VAL)) in
    if isVid = VAL andalso not (Option.isSome someVidInVe) then
      (ve, tsUnify, iUnify) else
      raise StaticInferenceFail "LAY PAT GET VID AS NOT VAL OR IN VEPAT" end

  and infTy c (VAR_TY tyvar) = let
    val isTyvar = C.isTyvar c tyvar in
    if isTyvar then let
      val varty = Option.valOf (C.getVarty c tyvar) in
      (VS.empty, TY.VARTY varty) end else
      raise StaticInferenceFail "TYVAR NOT EXPLICITLY DEFINED" end

    | infTy c (RCD_TY tyrow) = infTyrow c tyrow
    | infTy c (CON_TY (tyseq, ltycon)) = let
    val tsseq = List.map (fn ty => infTy c ty) tyseq
    val tsLtyconOp = C.getTystr c ltycon in
    if Option.isSome tsLtyconOp then let
      val (tf, _) = Option.valOf tsLtyconOp
      val tynameOp = C.getTyname c ltycon in
      if Option.isSome tynameOp then let
        val tyname = Option.valOf tynameOp
        val ts = TF.appTyname tsseq tyname in ts end
      else raise StaticInferenceFail "INVALID TYNAME" end
    else raise StaticInferenceFail "INVALID TYCON" end

    | infTy c (FUN_TY (argty, resty)) = let
    val tsArg = infTy c argty
    val tsRes = infTy c resty
    val tsFun = TS.getFunTysch tsArg tsRes in tsFun end


  and infTyrow   c ((lab, ty) :: tyrow) = let
    val tsTy = infTy c ty
    val tsTyrow = infTyrow c tyrow
    val ts = TS.insertRowTysch tsTyrow lab tsTy in ts end
    | infTyrow c [] = ISB.unitTysch

  fun inference prog = infDec ISB.context prog

end
