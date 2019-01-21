structure CoreSyntaxTree : CORE_SYNTAX_TREE = struct

  structure TC = TypeConstructor
  structure VID = ValueIdentifier
  structure SID = StructureIdentifier
  structure LVID = LongValueIdentifier
  structure LTC = LongTypeConstructor
  structure LSID = LongStructureIdentifier
  structure SC = SpecialConstant
  structure LA = ListAux
  structure OA = OptionAux

  datatype lab = datatype Lab.lab
  datatype scon = datatype SC.scon

  datatype atexp =
    SCON_ATEXP of scon |
    LVID_ATEXP of lvid |
    RCD_ATEXP  of exprow |
    LET_ATEXP  of dec * exp |
    EXP_ATEXP  of exp

  and exp =
    AT_EXP   of atexp |
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

  and exprowele = (lab * exp)
  and exprow = exprowele list
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
  and tyrowele = (lab * ty)
  and tyrow = tyrowele list

  and strdec = dec
  and topdec = strdec
  and prog = topdec


  exception FullSyntaxToCoreSyntaxException

  (* WARN: These transformations may cause problem *
   * target at preventing unnecessary construction *)
  fun atexpToExp (EXP_ATEXP e) = e
    | atexpToExp e = AT_EXP e

  fun expToAtexp (AT_EXP e) = e
    | expToAtexp e = EXP_ATEXP e

  fun atpatToPat (PAT_ATPAT p) = p
    | atpatToPat p = AT_PAT p

  fun patToAtpat (AT_PAT p) = p
    | patToAtpat p = PAT_ATPAT p

  fun fromAtexp (FST.SCON_ATEXP e) = SCON_ATEXP (fromScon e)
    | fromAtexp (FST.LVID_ATEXP e) = LVID_ATEXP e
    | fromAtexp (FST.RCD_ATEXP e) = RCD_ATEXP (fromExprow e)
    | fromAtexp (FST.LAB_ATEXP lab) =
    expToAtexp (fromExp(FST.labAtexpToFnExp (FST.LAB_ATEXP lab)))

    | fromAtexp FST.UNIT_ATEXP = RCD_ATEXP nil
    | fromAtexp (FST.TUP_ATEXP expseq) =
    fromAtexp (FST.tupAtexpToRcdAtexp (FST.TUP_ATEXP expseq))
    | fromAtexp (FST.LIST_ATEXP expseq) =
    expToAtexp (fromInfexp (FST.listAtexpToInfexp (FST.LIST_ATEXP expseq)))
    | fromAtexp (FST.SEQ_ATEXP expseq) =
    expToAtexp (fromAppexp (FST.seqAtexpToAppexp (FST.SEQ_ATEXP expseq)))
    | fromAtexp (FST.LET_ATEXP (dec, seqexp)) =
    LET_ATEXP (fromDec dec, fromExp (FST.seqAtexpToCaseExp (FST.SEQ_ATEXP seqexp)))

    | fromAtexp (FST.EXP_ATEXP exp) = expToAtexp (fromExp exp)

  and fromInfexp (FST.N_INFEXP (infexp, vid, infexp')) =
    INF_EXP(fromInfexp infexp, vid, fromInfexp infexp')

    | fromInfexp (FST.APP_INFEXP [atexp]) = atexpToExp (fromAtexp atexp)
    | fromInfexp (FST.APP_INFEXP [atexp, atexp']) =
    APP_EXP (atexpToExp (fromAtexp atexp'), fromAtexp atexp)
    | fromInfexp (FST.APP_INFEXP (atexp :: appexp)) =
    APP_EXP (fromInfexp (FST.APP_INFEXP appexp) ,fromAtexp atexp)
    | fromInfexp (FST.APP_INFEXP nil) = raise FullSyntaxToCoreSyntaxException

  and fromExprow exprow =
    map (fn (lab, exp) => (fromLab lab, fromExp exp)) exprow

  and fromAppexp (atexp :: nil) = atexpToExp (fromAtexp atexp)
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
    atexpToExp (fromAtexp (FST.whilExpToLetAtexp (FST.WHIL_EXP e)))
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
    patToAtpat (fromPat (FST.listAtpatToInfPat (FST.LIST_ATPAT patseq)))

    | fromAtpat (FST.PAT_ATPAT pat) = patToAtpat (fromPat pat)

  and fromPatrow patrow = let
      fun aux (FST.WILD_PATROW_ELE :: patrow) isWild = aux patrow true
      | aux (FST.LAB_PATROW_ELE (lab, pat) :: patrow) isWild =
      let val (fst, snd) = aux patrow isWild in
      ((fromLab lab, fromPat pat) :: fst ,snd) end
      | aux ((FST.VID_PATROW_ELE e) :: patrow) isWild =
      aux ((FST.vidPatroweleToLabPatrowele (FST.VID_PATROW_ELE e)) :: patrow) isWild
      | aux nil isWild = (nil, isWild)
    in aux patrow false end

  and fromPat (FST.AT_PAT atpat) = atpatToPat (fromAtpat atpat)
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

  fun atexpToString (SCON_ATEXP scon) = SC.toString scon
    | atexpToString (LVID_ATEXP lvid) = LVID.toString lvid
    | atexpToString (RCD_ATEXP exprow) = "{" ^ exprowToString exprow ^ "}"
    | atexpToString (LET_ATEXP (dec, exp)) =
    "LET " ^ (decToString dec) ^ "IN " ^ (expToString exp) ^ "END"
    | atexpToString (EXP_ATEXP exp) = expToString exp

  and expToString (AT_EXP atexp) = atexpToString atexp
    | expToString (APP_EXP (exp, atexp)) =
    (expToString exp) ^ " " ^ (atexpToString atexp)
    | expToString (INF_EXP (exp, vid, exp')) =
    (expToString exp) ^ " " ^ (vidToString vid) ^ " " ^(expToString exp')
    | expToString (TY_EXP (exp, ty)) = (expToString exp) ^ ":" ^ (tyToString ty)
    | expToString (HAND_EXP (exp, match)) =
    (expToString exp) ^ "HANDLE" ^ (matchToString match)
    | expToString (RAS_EXP exp) = "RAISE " ^ (expToString exp)
    | expToString (FN_EXP match) = "FN " ^ (matchToString match)

  and decToString (VAL_DEC (tyvarseq, valbind)) =
    "VAL " ^ (tyvarseqToString tyvarseq) ^ (valbindToString valbind)
    | decToString (TYP_DEC typbind) = "TYPE " ^ (typbindToString typbind)
    | decToString (DAT_DEC datbind) = "DATATYPE " ^ (datbindToString datbind)
    | decToString (REP_DEC (tycon, ltycon)) =
    "DATATYPE " ^ (tyconToString tycon) ^ "DATATYPE " ^ (ltyconToString ltycon)
    | decToString (ABS_DEC (datbind, dec)) =
    "ABSTYPE " ^ (datbindToString datbind) ^ "WITH " ^ (decToString dec) ^ "END"
    | decToString (EXC_DEC (exbind)) = "EXCEPTION " ^ exbindToString exbind
    | decToString (LOC_DEC (dec, dec')) =
    "LOCAL " ^ (decToString dec) ^ " IN " ^ (decToString dec') ^ "END"
    | decToString (OPEN_DEC (lstridseq)) =
    "OPEN " ^ (lstridseqToString lstridseq)
    | decToString (SEQ_DEC (dec, dec')) =
    (decToString dec) ^ ";\n" ^ (decToString dec')
    | decToString (INF_DEC (intOption, vidseq)) =
    "INFIX " ^ (OA.toString intOption Int.toString "") ^
    (vidseqToString vidseq)
    | decToString (INFR_DEC (intOption, vidseq)) =
    "INFIXR " ^ (OA.toString intOption Int.toString "") ^
    (vidseqToString vidseq)
    | decToString (NOF_DEC vidseq) =
    "NONFIX " ^ (vidseqToString vidseq)

  and valbindToString (NRE_VALBIND vrow) = vrowToString vrow
    | valbindToString (REC_VALBIND vrow) = "REC " ^ (vrowToString vrow)

  and exbindeleToString (VID_EXBIND_ELE (vid, tyOption)) =
    (vidToString vid) ^ (OA.toString tyOption tyToString "")
    | exbindeleToString (REP_EXBIND_ELE (vid, lvid)) =
    (vidToString vid) ^ (LVID.toString lvid)

  and atpatToString WILD_ATPAT = "_"
    | atpatToString (SCON_ATPAT scon) = SC.toString scon
    | atpatToString (LVID_ATPAT lvid) = LVID.toString lvid
    | atpatToString (RCD_ATPAT patrow) =
    "{" ^ (patrowToString patrow) ^ "}"
    | atpatToString (PAT_ATPAT pat) = patToString pat

  and patToString (AT_PAT atpat) = atpatToString atpat
    | patToString (CON_PAT (lvid, atpat)) =
    (LVID.toString lvid) ^ (atpatToString atpat)
    | patToString (INF_PAT (pat, vid, pat')) =
    (patToString pat) ^ (vidToString vid) ^ (patToString pat')
    | patToString (TY_PAT (pat, ty)) = (patToString pat) ^ " : " ^ (tyToString ty)
    | patToString (LAY_PAT (vid, tyOption, pat)) =
    (vidToString vid) ^ ":" ^ (OA.toString tyOption tyToString " ") ^
    (patToString pat)

  and tyToString (VAR_TY tyvar) = tyvarToString tyvar
    | tyToString (RCD_TY tyrow) = tyrowToString tyrow
    | tyToString (CON_TY (tyseq, ltycon)) =
    (tyseqToString tyseq) ^ (ltyconToString ltycon)
    | tyToString (FUN_TY (ty, ty')) = (tyToString ty) ^ " -> " ^ (tyToString ty')


  and idToString id = id
  and tyvarToString tyvar = "'" ^ tyvar
  and vidToString vid = vid
  and stridToString strid = strid
  and tyconToString tycon = tycon
  and ltyconToString ltycon = lidToString ltycon
  and lvidToString lvid = lidToString lvid
  and lstridToString lstrid = lidToString lstrid

  and lidpreToString lidpre = LA.toString lidpre stridToString "."
  and lidToString (lidpre, vid) = (lidpreToString lidpre) ^
    (if length lidpre > 0 then "." else "") ^ (vidToString vid)

  and lstridseqToString lstridseq = (LA.toString lstridseq LSID.toString " ")
  and vidseqToString vidseq = LA.toString vidseq vidToString " "
  and vroweleToString (pat, exp) = (patToString pat) ^ " = " ^ (expToString exp)
  and vrowToString vrow = LA.toString vrow vroweleToString "AND"
  and tyseqToString tyseq =
    (LA.toString tyseq tyToString " ") ^
    (if List.null tyseq then "" else " ")
  and tyvarseqToString tyvarseq =
    (LA.toString tyvarseq tyvarToString " ") ^
    (if List.null tyvarseq then "" else " ")
  and exproweleToString (lab, exp) =
    (Lab.toString lab) ^ " = " ^ (expToString exp)
  and exprowToString exprow = LA.toString exprow exproweleToString ","
  and mruleToString (pat, exp) =
    (patToString pat) ^ " => " ^ (expToString exp) ^ ""
  and matchToString match = LA.toString match mruleToString "\n|"
  and typbindeleToString (tyvarseq, tycon, ty) =
    (tyvarseqToString tyvarseq) ^ (tyconToString tycon) ^ " = " ^ (tyToString ty)
  and typbindToString typbind = LA.toString typbind typbindeleToString "and"
  and conbindeleToString (vid, tyOption) =
    (vidToString vid) ^ " OF " ^ (OA.toString tyOption tyToString "<>")
  and conbindToString conbind = LA.toString conbind conbindeleToString " |\n"
  and datbindeleToString (tyvarseq, tycon, conbind) =
    (tyvarseqToString tyvarseq) ^ (tyconToString tycon) ^
    " =\n" ^ (conbindToString conbind)
  and datbindToString datbind = LA.toString datbind datbindeleToString "and"
  and exbindToString exbind = LA.toString exbind exbindeleToString "and"
  and patroweleToString (lab, pat) = (Lab.toString lab) ^ " = " ^ (patToString pat)
  and patrowToString (patrow, isWild) = LA.toString patrow patroweleToString ","
    ^ (if isWild then ",..." else "")
  and tyroweleToString (lab, ty) = (Lab.toString lab) ^ " : " ^ (tyToString ty)
  and tyrowToString tyrow =
    "{" ^ (LA.toString tyrow tyroweleToString ", ") ^ "}"
  and strdecToString dec = decToString dec
  and topdecToString strdec = strdecToString strdec
  and progToString topdec = topdecToString topdec ^ "\n"

end

structure CST = CoreSyntaxTree
