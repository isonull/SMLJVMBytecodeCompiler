structure CoreSyntaxTree : CORE_SYNTAX_TREE = struct

  datatype atexp =
    SCON_ATEXP of scon |
    LVID_ATEXP of lvid |
    RCD_ATEXP of exprow |
    LET_ATEXP of dec * exp |
    EXP_ATEXP of exp

  and exp =
    AT_EXP of atexp |
    APP_EXP  of exp * atexp |
    INF_EXP  of exp * vid * exp |
    TY_EXP   of exp * ty |
    HAND_EXP of exp * match |
    RAS_EXP  of exp |
    FN_EXP   of match

  and dec =
    VAL_DEC of tyvarseq * valbind |
    TYP_DEC of typbind |
    DAT_DEC of datbind |
    REP_DEC of tycon * ltycon |
    ABS_DEC of datbind * dec |
    EXC_DEC of exbind |
    LOC_DEC of dec * dec |
    OPEN_DEC of lstridseq |
    SEQ_DEC of dec * dec |
    INF_DEC of int option * vidseq |
    INFR_DEC of int option * vidseq |
    NOF_DEC of vidseq

  and valbind =
    NRE_VALBIND of vrow |
    REC_VALBIND of vrow

  and exbindele =
    VID_EXBIND_ELE of vid * ty option |
    REP_EXBIND_ELE of vid * lvid

  and atpat =
    WILD_ATPAT |
    SCON_ATPAT of scon |
    LVID_ATPAT of lvid |
    RCD_ATPAT of patrow |
    PAT_ATPAT of pat

  and pat =
    AT_PAT of atpat |
    CON_PAT of lvid * atpat |
    INF_PAT of pat * vid * pat |
    TY_PAT of pat * ty |
    LAY_PAT of vid * ty option * pat

  and ty =
    VAR_TY of tyvar |
    RCD_TY of tyrow |
    CON_TY of tyseq * ltycon |
    FUN_TY of ty * ty

  and scon =
    INT_SCON of int |
    REAL_SCON of real |
    WORD_SCON of word |
    CHAR_SCON of char |
    STR_SCON of string

  and lab =
    INT_LAB of int |
    STR_LAB of string

  withtype id = string
  and tyvar = string

  and vid = id
  and strid = id
  and tycon = id

  and lidpre = strid list
  and 'a lid = lidpre * 'a
  and lvid = vid lid
  and lstrid = strid lid
  and ltycon = tycon lid

  and lstridseq = lstrid list
  and vidseq = vid list
  and vrowele = (pat * exp)
  and vrow = vrowele list
  and tyseq = ty list
  and tyvarseq = tyvar list

  and exprow = (lab * exp) list
  and mrule = pat * exp
  and match = mrule list
  and typbindele = (tyvarseq * tycon * ty)
  and typbind = typbindele list
  and conbindele = (vid * ty option)
  and conbind = conbindele list
  and datbindele = (tyvarseq * tycon * conbind)
  and datbind = datbindele list
  and exbind = exbindele list
  and patrowele = lab * pat
  and patrow = patrowele list * bool
  and tyrow = (lab * ty) list

  and strdec = dec
  and topdec = strdec
  and prog = topdec

  exception FullSyntaxToCoreSyntaxException

  fun fromAtexp (FST.SCON_ATEXP e) = SCON_ATEXP (fromScon e)
    | fromAtexp (FST.LVID_ATEXP e) = LVID_ATEXP e
    | fromAtexp (FST.RCD_ATEXP e) = RCD_ATEXP (fromExprow e)
    | fromAtexp (FST.LAB_ATEXP lab) =
    EXP_ATEXP (fromExp(FST.labAtexpToFnExp (FST.LAB_ATEXP lab)))

    | fromAtexp FST.UNIT_ATEXP = RCD_ATEXP nil
    | fromAtexp (FST.TUP_ATEXP expseq) =
    fromAtexp (FST.tupAtexpToRcdAtexp (FST.TUP_ATEXP expseq))
    | fromAtexp (FST.LIST_ATEXP expseq) =
    EXP_ATEXP (fromInfexp (FST.listAtexpToInfexp (FST.LIST_ATEXP expseq)))
    | fromAtexp (FST.SEQ_ATEXP expseq) =
    EXP_ATEXP (fromAppexp (FST.seqAtexpToAppexp (FST.SEQ_ATEXP expseq)))
    | fromAtexp (FST.LET_ATEXP (dec, seqexp)) =
    LET_ATEXP (fromDec dec, fromExp (FST.seqAtexpToCaseExp (FST.SEQ_ATEXP seqexp)))

    | fromAtexp (FST.EXP_ATEXP exp) = EXP_ATEXP (fromExp exp)

  and fromInfexp (FST.N_INFEXP (infexp, vid, infexp')) =
    INF_EXP(fromInfexp infexp, vid, fromInfexp infexp')
    | fromInfexp (FST.APP_INFEXP (atexp :: nil)) = AT_EXP (fromAtexp atexp)
    | fromInfexp (FST.APP_INFEXP (atexp :: appexp)) =
    APP_EXP (fromInfexp (FST.APP_INFEXP appexp) ,fromAtexp atexp)
    | fromInfexp (FST.APP_INFEXP nil) = raise FullSyntaxToCoreSyntaxException

  and fromExprow exprow =
    map (fn (lab, exp) => (fromLab lab, fromExp exp)) exprow

  and fromAppexp (atexp :: nil) = AT_EXP (fromAtexp atexp)
    | fromAppexp (atexp :: atexps) = APP_EXP (fromAppexp atexps, fromAtexp atexp)
    | fromAppexp _ = raise FullSyntaxToCoreSyntaxException

  and fromExp (FST.INF_EXP e) = fromInfexp e
    | fromExp (FST.TY_EXP (exp, ty)) = TY_EXP (fromExp exp, fromTy ty)
    | fromExp (FST.AND_EXP e) = fromAppexp (FST.andExpToAppexp (FST.AND_EXP e))
    | fromExp (FST.OR_EXP e) = fromAppexp (FST.orExpToAppexp (FST.OR_EXP e))
    | fromExp (FST.HAND_EXP (e, m)) = HAND_EXP(fromExp e, fromMatch m)
    | fromExp (FST.RAS_EXP e) = RAS_EXP (fromExp e)
    | fromExp (FST.IF_EXP e)  = fromAppexp (FST.ifExpToAppexp (FST.IF_EXP e))
    | fromExp (FST.WHIL_EXP e) =
    AT_EXP (fromAtexp (FST.whilExpToLetAtexp (FST.WHIL_EXP e)))
    | fromExp (FST.CASE_EXP e) =
    fromAppexp (FST.caseExpToAppexp (FST.CASE_EXP e))
    | fromExp (FST.FN_EXP m) = FN_EXP (fromMatch m)

  and fromMatch match = map fromMrule match

  and fromMrule (pat, exp) = (fromPat pat, fromExp exp)

  and fromDec (FST.VAL_DEC (tyvarseq, valbind)) =
    VAL_DEC (tyvarseq, fromValbind valbind)
    | fromDec (FST.FUN_DEC e) = fromDec (FST.funDecToValDec (FST.FUN_DEC e))
    | fromDec (FST.TYP_DEC typbind) = TYP_DEC (fromTypbind typbind)
    | fromDec (FST.DAT_DEC (datbind, SOME typbind)) =
    fromDec (FST.datDecReduce (FST.DAT_DEC (datbind, SOME typbind)))

    | fromDec (FST.DAT_DEC (datbind, NONE)) = DAT_DEC (fromDatbind datbind)
    | fromDec (FST.REP_DEC (tycon, ltycon)) = REP_DEC (tycon, ltycon)
    | fromDec (FST.ABS_DEC absbind) =
    let
      fun aux (FST.ABS_DEC (datbind, NONE, dec)) = (datbind, dec)
        | aux _ = raise FullSyntaxToCoreSyntaxException
      val (datbind, dec) = aux (FST.absDecReduce (FST.ABS_DEC absbind))
    in ABS_DEC (fromDatbind datbind, fromDec dec) end

    | fromDec (FST.EXC_DEC exbind) = EXC_DEC (fromExbind exbind)
    | fromDec (FST.LOC_DEC (dec, dec')) = LOC_DEC (fromDec dec, fromDec dec')
    | fromDec (FST.OPEN_DEC (lstridseq)) = OPEN_DEC (lstridseq)
    | fromDec (FST.SEQ_DEC (dec, dec')) = SEQ_DEC (fromDec dec, fromDec dec')
    | fromDec (FST.INF_DEC (intOption, vidseq)) =
    INF_DEC (intOption, vidseq)

    | fromDec (FST.INFR_DEC (intOption, vidseq)) =
    INFR_DEC (intOption, vidseq)

    | fromDec (FST.NOF_DEC vidseq) = NOF_DEC vidseq

  and fromValbind (FST.NRE_VALBIND vrow) = NRE_VALBIND (fromVrow vrow)
    | fromValbind (FST.REC_VALBIND vrow) = REC_VALBIND (fromVrow vrow)

  and fromVrow vrow = map (fn (pat, exp) => (fromPat pat, fromExp exp)) vrow

  and fromTypbind ((tyvarseq, tycon, ty) :: typbind) =
    (tyvarseq, tycon, fromTy ty) :: (fromTypbind typbind)
    | fromTypbind nil = nil

  and fromDatbind ((tyvarseq, tycon, conbind) :: datbind) =
    (tyvarseq, tycon, fromConbind conbind) :: (fromDatbind datbind)
    | fromDatbind nil = nil

  and fromConbind ((vid, tyOption) :: conbind) =
    (vid, Option.map fromTy tyOption) :: (fromConbind conbind)
    | fromConbind nil = nil

  and fromExbind ((FST.VID_EXBIND_ELE (vid, tyOption)) :: exbind) =
    (VID_EXBIND_ELE (vid, Option.map fromTy tyOption)) :: (fromExbind exbind)

    | fromExbind ((FST.REP_EXBIND_ELE (vid, lvid)) :: exbind) =
    (REP_EXBIND_ELE (vid, lvid)) :: (fromExbind exbind)
    | fromExbind nil = nil

  and fromAtpat FST.WILD_ATPAT = WILD_ATPAT
    | fromAtpat (FST.SCON_ATPAT scon) = SCON_ATPAT (fromScon scon)
    | fromAtpat (FST.LVID_ATPAT lvid) = LVID_ATPAT lvid
    | fromAtpat (FST.RCD_ATPAT patrow) = RCD_ATPAT (fromPatrow patrow)
    | fromAtpat FST.UNIT_ATPAT = RCD_ATPAT (nil, false)
    | fromAtpat (FST.TUP_ATPAT patseq) =
    fromAtpat (FST.tupAtpatToRcdAtpat (FST.TUP_ATPAT patseq))

    | fromAtpat (FST.LIST_ATPAT patseq) =
    PAT_ATPAT (fromPat (FST.listAtpatToInfPat (FST.LIST_ATPAT patseq)))

    | fromAtpat (FST.PAT_ATPAT pat) = PAT_ATPAT (fromPat pat)

  and fromPatrow patrow = let
      fun aux (FST.WILD_PATROW_ELE :: patrow) isWild = aux patrow true
      | aux (FST.LAB_PATROW_ELE (lab, pat) :: patrow) isWild =
      let val (fst, snd) = aux patrow isWild in
      ((fromLab lab, fromPat pat) :: fst ,snd) end
      | aux ((FST.VID_PATROW_ELE e) :: patrow) isWild =
      aux ((FST.vidPatroweleToLabPatrowele (FST.VID_PATROW_ELE e)) :: patrow) isWild
      | aux nil isWild = (nil, isWild)
    in aux patrow false end

  and fromPat (FST.AT_PAT atpat) = AT_PAT (fromAtpat atpat)
    | fromPat (FST.CON_PAT (lvid, atpat)) = CON_PAT (lvid, fromAtpat atpat)
    | fromPat (FST.INF_PAT (pat, vid, pat')) =
    INF_PAT (fromPat pat, vid, fromPat pat')
    | fromPat (FST.TY_PAT (pat, ty)) = TY_PAT (fromPat pat, fromTy ty)
    | fromPat (FST.LAY_PAT (vid, tyOption, pat)) =
    LAY_PAT (vid, Option.map fromTy tyOption, fromPat pat)

  and fromTy (FST.VAR_TY tyvar) = VAR_TY tyvar
    | fromTy (FST.RCD_TY tyrow) = RCD_TY (fromTyrow tyrow)
    | fromTy (FST.CON_TY (tyseq, ltycon)) = CON_TY (map fromTy tyseq, ltycon)
    | fromTy (FST.TUP_TY tyseq) = fromTy (FST.tupTyToRcdTy (FST.TUP_TY tyseq))
    | fromTy (FST.FUN_TY (ty, ty')) = FUN_TY (fromTy ty, fromTy ty')

  and fromTyrow tyrow = map (fn (lab, ty) => (fromLab lab, fromTy ty)) tyrow

  and fromScon (FST.INT_SCON  c) = INT_SCON  c
    | fromScon (FST.REAL_SCON c) = REAL_SCON c
    | fromScon (FST.WORD_SCON c) = WORD_SCON c
    | fromScon (FST.CHAR_SCON c) = CHAR_SCON c
    | fromScon (FST.STR_SCON  c) = STR_SCON  c

  and fromLab (FST.INT_LAB l) = INT_LAB l
    | fromLab (FST.STR_LAB l) = STR_LAB l

  and fromProg prog = fromDec prog
end

structure CST = CoreSyntaxTree
