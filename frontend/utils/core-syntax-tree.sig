signature CORE_SYNTAX_TREE = sig

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

  val listToString : 'a list -> ('a -> string) -> string -> string
  val optionToString : 'a option -> ('a -> string) -> string -> string

  val fromAtexp : FST.atexp -> atexp
  val fromInfexp : FST.infexp -> exp
  val fromExprow : FST.exprow -> exprow
  val fromAppexp : FST.appexp -> exp
  val fromExp : FST.exp -> exp
  val fromMatch : FST.match -> match
  val fromMrule : FST.mrule -> mrule
  val fromDec : FST.dec -> dec

  val fromValbind : FST.valbind -> valbind
  val fromTypbind : FST.typbind -> typbind
  val fromDatbind : FST.datbind -> datbind
  val fromConbind : FST.conbind -> conbind
  val fromExbind : FST.exbind -> exbind

  val fromAtpat : FST.atpat -> atpat
  val fromPatrow : FST.patrow -> patrow
  val fromPat : FST.pat -> pat
  val fromTy : FST.ty -> ty
  val fromTyrow : FST.tyrow -> tyrow

  val fromLab : FST.lab -> lab
  val fromScon : FST.scon -> scon
  val fromProg : FST.prog -> prog
  val progToString : prog -> string

end
