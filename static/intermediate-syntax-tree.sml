structure IntermediateSyntaxTree = struct
  datatype scon = datatype SpecialConstant.scon
  type lvid = LongValueIdentifier.lvid
  datatype lab = datatype Lab.lab

  datatype atexp =
    SCON_ATEXP of scon |
    LVID_ATEXP of lvid |
    RCD_ATEXP  of exprow |
    LET_ATEXP  of dec * exp |
    EXP_ATEXP  of exp

  and exp =
    AT_EXP   of atexp |
    APP_EXP  of exp * atexp |
    HAND_EXP of exp * match |
    RAS_EXP  of exp |
    FN_EXP   of match

  and dec =
    VAL_DEC of valbind |
    DAT_DEC of datbind |
    EXC_DEC of exbind |
    LOC_DEC of dec * dec |
    SEQ_DEC of dec list

  and valbind =
    NRE_VALBIND of vrow |
    REC_VALBIND of vrow

  and atpat =
    WILD_ATPAT |
    SCON_ATPAT of scon |
    LVID_ATPAT of lvid |
    RCD_ATPAT of patrow |
    PAT_ATPAT of pat

  and pat =
    AT_PAT of atpat |
    CON_PAT of lvid * atpat |
    LAY_PAT of vid * pat

  withtype id = string

  and isfun = bool
  and vid = id

  and vrow = (pat * exp) list

  and exprow = (lab * exp) list
  and mrule = pat * exp
  and match = mrule list
  and conbind = (vid * isfun) list
  and datbind = conbind list
  and exbind = (vid * isfun) list
  and patrow = (lab * pat) list
  and strdec = dec
  and topdec = strdec
  and prog = topdec

  fun nullSeqdec (SEQ_DEC decs) = List.null decs
    | nullSeqdec _ = raise Match

  fun regSeqdec (SEQ_DEC [dec]) = dec
    | regSeqdec (SEQ_DEC ds) = SEQ_DEC ds
    | regSeqdec _ = raise Match

  fun concatSeqdec (SEQ_DEC decs1) (SEQ_DEC decs2) = SEQ_DEC (decs1 @ decs2)
    | concatSeqdec _ _ = raise Match

  fun toSeqdec (SEQ_DEC dec) = (SEQ_DEC dec)
    | toSeqdec dec = SEQ_DEC [dec]

  fun decCombine dec1 dec2 = let
    val seqdec = concatSeqdec (toSeqdec dec1) (toSeqdec dec2) in
    regSeqdec seqdec end

  structure TC = TypeConstructor
  structure VID = ValueIdentifier
  structure SID = StructureIdentifier
  structure LVID = LongValueIdentifier
  structure LTC = LongTypeConstructor
  structure LSID = LongStructureIdentifier
  structure SC = SpecialConstant
  structure LA = ListAux
  structure OA = OptionAux

  fun atexpToString (SCON_ATEXP scon) = SC.toString scon
    | atexpToString (LVID_ATEXP lvid) = LVID.toString lvid
    | atexpToString (RCD_ATEXP exprow) = "{" ^ exprowToString exprow ^ "}"
    | atexpToString (LET_ATEXP (dec, exp)) =
    "LET " ^ (decToString dec) ^ "IN " ^ (expToString exp) ^ "END"
    | atexpToString (EXP_ATEXP exp) = expToString exp

  and expToString (AT_EXP atexp) = atexpToString atexp
    | expToString (APP_EXP (exp, atexp)) =
    (expToString exp) ^ " " ^ (atexpToString atexp)
    | expToString (HAND_EXP (exp, match)) =
    (expToString exp) ^ "HANDLE" ^ (matchToString match)
    | expToString (RAS_EXP exp) = "RAISE " ^ (expToString exp)
    | expToString (FN_EXP match) = "FN " ^ (matchToString match)

  and decToString (VAL_DEC valbind) =
    "VAL " ^ (valbindToString valbind)
    | decToString (DAT_DEC datbind) = "DATATYPE " ^ (datbindToString datbind)
    | decToString (EXC_DEC exbind)  = "EXCEPTION " ^ exbindToString exbind
    | decToString (LOC_DEC (dec, dec')) =
    "LOCAL " ^ (decToString dec) ^ " IN " ^ (decToString dec') ^ "END"
    | decToString (SEQ_DEC decs) = LA.toString decs decToString ";\n"

  and valbindToString (NRE_VALBIND vrow) = vrowToString vrow
    | valbindToString (REC_VALBIND vrow) = "REC " ^ (vrowToString vrow)

  and vrowToString vrow = LA.toString vrow 
    (fn (pat, exp) => (patToString pat) ^ " = " ^ (expToString exp) ) "AND\n "

  and atpatToString WILD_ATPAT = "_"
    | atpatToString (SCON_ATPAT scon) = SC.toString scon
    | atpatToString (LVID_ATPAT lvid) = LVID.toString lvid
    | atpatToString (RCD_ATPAT patrow) =
    "{" ^ (patrowToString patrow) ^ "}"
    | atpatToString (PAT_ATPAT pat) = patToString pat

  and patToString (AT_PAT atpat) = atpatToString atpat
    | patToString (CON_PAT (lvid, atpat)) =
    (LVID.toString lvid) ^ (atpatToString atpat)
    | patToString (LAY_PAT (vid, pat)) = vid ^ " AS " ^ (patToString pat)

  and exprowToString exprow = LA.toString exprow 
    (fn (lab, exp) => (Lab.toString lab) ^ " = " ^(expToString exp)) ","

  and mruleToString (pat, exp) =
    (patToString pat) ^ " => " ^ (expToString exp) ^ ""

  and matchToString match = LA.toString match mruleToString "\n| "

  and conbindToString conbind = LA.toString conbind conToString " | "

  and conToString (vid, isfun) = vid ^ (if isfun then "$" else "")

  and datbindToString datbind = LA.toString datbind conbindToString " and\n"

  and exbindToString exbind = LA.toString exbind conToString " | "

  and patrowToString patrow = LA.toString patrow 
    (fn (lab, pat) => (Lab.toString lab) ^ " = " ^ (patToString pat)) ","

  and strdecToString dec = decToString dec
  and topdecToString strdec = strdecToString strdec
  and progToString topdec = topdecToString topdec ^ "\n"

  val toString = progToString

end
