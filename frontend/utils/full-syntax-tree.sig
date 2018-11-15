signature FULL_SYNTAX_TREE = sig

  datatype atexp =
    SCON_ATEXP of scon |
    LVID_ATEXP of lvid |
    RCD_ATEXP of exprow |
    LAB_ATEXP of lab |
    UNIT_ATEXP |
    TUP_ATEXP of expseq |
    LIST_ATEXP of expseq |
    SEQ_ATEXP of expseq |
    LET_ATEXP of dec * expseq |
    EXP_ATEXP of exp

  and infexp =
    N_INFEXP of infexp * vid * infexp |
    APP_INFEXP of appexp

  and exp =
    INF_EXP  of infexp |
    TY_EXP   of exp * ty |
    AND_EXP  of exp * exp |
    OR_EXP   of exp * exp |
    HAND_EXP of exp * match |
    RAS_EXP  of exp |
    IF_EXP   of exp * exp * exp |
    WHIL_EXP of exp * exp |
    CASE_EXP of exp * match |
    FN_EXP   of match

  and dec =
    VAL_DEC of tyvarseq * valbind |
    FUN_DEC of tyvarseq * fvalbind |
    TYP_DEC of typbind |
    DAT_DEC of datbind * typbind option |
    REP_DEC of tycon * ltycon |
    ABS_DEC of datbind * typbind option * dec |
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
    UNIT_ATPAT |
    TUP_ATPAT of patseq |
    LIST_ATPAT of patseq |
    PAT_ATPAT of pat

  and patrowele =
    WILD_PATROW_ELE |
    LAB_PATROW_ELE of lab * pat |
    VID_PATROW_ELE of vid * ty option * pat option

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
    TUP_TY of tyseq |
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
  and expseq = exp list
  and decseq = dec list
  and vrowele = (pat * exp)
  and vrow = vrowele list
  and frow = atpat list
  and fvalele = vid * frow * exp
  and fval = fvalele list
  and patseq = pat list
  and tyseq = ty list
  and tyvarseq = tyvar list

  and exprow = (lab * exp) list
  and appexp = atexp list
  and mrule = pat * exp
  and match = mrule list
  and fvalbind = fval list
  and typbindele = (tyvarseq * tycon * ty)
  and typbind = typbindele list
  and conbindele = (vid * ty option)
  and conbind = conbindele list
  and datbindele = (tyvarseq * tycon * conbind)
  and datbind = datbindele list
  and exbind = exbindele list
  and patrow = patrowele list
  and tyrow = (lab * ty) list

  and strdec = dec
  and topdec = strdec
  and prog = topdec

  exception DerivedFormException

  val patById : id -> pat
  val wildPat : pat

  val lidById : id -> id lid
  val expById : id -> exp
  val expByAppexp : appexp -> exp
  val expByAtexp : atexp -> exp

  val getUniqueId : unit -> id
  val getUniqueIdseq : int -> id list

  val unitAtexpToRcdAtexp : atexp -> atexp
  val tupAtexpToRcdAtexp : atexp -> atexp
  val labAtexpToFnExp : atexp -> exp

  val caseExpToAppexp : exp -> appexp
  val ifExpToCaseExp : exp -> exp
  val orExpToIfExp : exp -> exp
  val andExpToIfExp : exp -> exp

  val seqAtexpToCaseExp : atexp -> exp
  val whilExpToLetAtexp : exp -> atexp
  val listAtexpToInfexp : atexp -> infexp

  val unitAtpatToRcdAtpat : atpat -> atpat
  val tupAtpatToRcdAtpat : atpat -> atpat
  val listAtpatToInfPat : atpat -> pat

  val vidPatroweleToLabPatrowele : patrowele -> patrowele

  val tupTyToRcdTy : ty -> ty
  val frowToTupAtpat : frow -> atpat
  val fvaleleToMrule : fvalele -> mrule
  val fvalToVrowele : fval -> vrowele
  val fvalbindToValbind : fvalbind -> valbind

  val funDecToValDec : dec -> dec
  val datDecReduce : dec -> dec
  val absDecReduce : dec -> dec

  val andExpToAppexp : exp -> appexp
  val orExpToAppexp : exp -> appexp
  val ifExpToAppexp : exp -> appexp
  val seqAtexpToAppexp : atexp -> appexp
end
