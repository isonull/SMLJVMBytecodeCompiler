structure IntermediateSyntaxTree = struct
  structure TS = TypeScheme
  datatype scon = datatype SpecialConstant.scon
  type lvid = LongValueIdentifier.lvid
  datatype lab = datatype Lab.lab
  type ts = TypeScheme.tysch

  datatype atexp =
    SCON_ATEXP of scon |
    LVID_ATEXP of lvid |
    RCD_ATEXP  of exprow |
    LET_ATEXP  of dec * expty |
    EXP_ATEXP  of expty

  and exp =
    AT_EXP   of atexpty |
    APP_EXP  of expty * atexpty |
    HAND_EXP of expty * match |
    RAS_EXP  of expty |
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
    PAT_ATPAT of patty

  and pat =
    AT_PAT of atpatty |
    CON_PAT of (lvid * ts) * atpatty |
    LAY_PAT of vid * patty

  withtype expty = exp * ts
  and atexpty = atexp * ts
  and patty = pat * ts
  and atpatty = atpat * ts

  and id = string

  and isfun = bool
  and vid = id

  and vrow = ((patty * expty) list * bool)
  and exprow = (lab * expty) list
  and mrule = patty * expty
  and match = (mrule list * bool) (* is complete *)
  and conbind = (vid * isfun) list
  and datbind = conbind list
  and exbind = (vid * isfun) list
  and patrow = (lab * patty) list
  and strdec = dec
  and topdec = strdec
  and prog = topdec

  val valOf = Option.valOf

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
    "LET " ^ (decToString dec) ^ "IN " ^ (expToString (#1 exp)) ^ "END"
    | atexpToString (EXP_ATEXP exp) = expToString (#1 exp)

  and expToString (AT_EXP atexp) = atexpToString (#1 atexp)
    | expToString (APP_EXP (exp, atexp)) =
    (expToString (#1 exp)) ^ " " ^ (atexpToString (#1 atexp))
    | expToString (HAND_EXP (exp, match)) =
    (expToString (#1 exp)) ^ "HANDLE" ^ (matchToString match)
    | expToString (RAS_EXP exp) = "RAISE " ^ (expToString (#1 exp))
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

  and vrowToString (vrow, comp) = LA.toString vrow
    (fn (pat, exp) => (patToString (#1 pat)) ^ " = " ^ (expToString (#1 exp)) ) "AND\n "

  and atpatToString WILD_ATPAT = "_"
    | atpatToString (SCON_ATPAT scon) = SC.toString scon
    | atpatToString (LVID_ATPAT lvid) = LVID.toString lvid
    | atpatToString (RCD_ATPAT patrow) =
    "{" ^ (patrowToString patrow) ^ "}"
    | atpatToString (PAT_ATPAT pat) = patToString (#1 pat)

  and patToString (AT_PAT atpat) = atpatToString (#1 atpat)
    | patToString (CON_PAT (lvid, atpat)) =
    (LVID.toString (#1 lvid)) ^ (atpatToString (#1 atpat))
    | patToString (LAY_PAT (vid, pat)) = vid ^ " AS " ^ (patToString (#1 pat))

  and exprowToString exprow = LA.toString exprow
    (fn (lab, exp) => (Lab.toString lab) ^ " = " ^(expToString (#1 exp))) ","

  and mruleToString (pat, exp) =
    (patToString (#1 pat)) ^ " => " ^ (expToString (#1 exp)) ^ ""

  and matchToString (match, _) = LA.toString match mruleToString "\n| "

  and conbindToString conbind = LA.toString conbind conToString " | "

  and conToString (vid, isfun) = vid ^ (if isfun then "$" else "")

  and datbindToString datbind = LA.toString datbind conbindToString " and\n"

  and exbindToString exbind = LA.toString exbind conToString " | "

  and patrowToString patrow = LA.toString patrow
    (fn (lab, pat) => (Lab.toString lab) ^ " = " ^ (patToString (#1 pat))) ","

  and strdecToString dec = decToString dec
  and topdecToString strdec = strdecToString strdec
  and progToString topdec = topdecToString topdec ^ "\n"

  val toString = progToString

  exception Goto
  datatype ty = datatype Type.ty
  structure LM = LabBinaryMap
  structure LS = LabBinarySet

  val a = VARTY (0, false)
  val tswild = TS.wild

  fun fillAtexp tss (atexp, ts) = let
    val (ae, fin) = case atexp of
      SCON_ATEXP s => (SCON_ATEXP s, true)
    | LVID_ATEXP l => (LVID_ATEXP l, true)
    | RCD_ATEXP r => let 
       in
      (RCD_ATEXP (List.map (fn (lab, exp) => let
        val tss' = List.foldl 
          (fn ((c, ROWTY (rt, true)), tss') => (
            ((c, valOf (LM.find (rt, lab))) :: tss')
            handle Option => tss')
            | (_, tss') => tss') [] (ts :: tss)
        val (exp', fin) = fillExp tss' exp in 
        if fin then (lab, exp') else raise Goto end) r), true)
      handle Goto => (RCD_ATEXP r, false) end
    | LET_ATEXP (dec, exp) => let 
      val (exp', fin1) = (fillExp (ts :: tss) exp) 
      val (dec', fin2) = (fillDec dec) in
      (LET_ATEXP (dec', exp'), fin1 andalso fin2) end
    | EXP_ATEXP exp => let
      val (exp', fin)  = fillExp (ts :: tss) exp in 
      (EXP_ATEXP exp', fin) end in 
     ((ae, ts), fin)  end

  and fillExp tss (exp, ts) = let 
    val (exp', fin) = case exp of
      AT_EXP   atexp => let
      val (atexp', fin) = fillAtexp (ts :: tss) atexp in
      (AT_EXP atexp', fin) end
    | APP_EXP  (exp, atexp) => let
      val tsatexp = #2 exp
      val tssexp = List.map (fn (c, resty) => (c, FUNTY(a, resty))) (ts :: tss)
      val tsexp = (#1 (#2 atexp),FUNTY (#2 (#2 atexp), a))
      val (exp', fin1) = fillExp (tsexp :: tssexp) exp
      val (atexp', fin2) = fillAtexp [tsatexp] atexp in
      (APP_EXP (exp', atexp'), true) end
    | HAND_EXP _ => raise Match
    | RAS_EXP  (exp) => let
        val (exp', fin) = fillExp (ts :: tss) exp in
        (RAS_EXP exp', fin) end
    | FN_EXP   (match, comp) => let 
      val tssarg = List.foldl (
        fn ((c, FUNTY (a, b)), tss') => (c, a) :: tss'
          | (_, tss') => tss' ) [] (ts :: tss) 
      val tssres = List.foldl (
        fn ((c, FUNTY (a, b)), tss') => (c, b) :: tss'
          | (_, tss') => tss' ) [] (ts :: tss)   in
      (FN_EXP (List.map (fn (pat, exp) => let
        val (pat', fin1) = fillPat tssarg pat
        val (exp', fin2) = fillExp tssres exp in
        if fin1 andalso fin2 then (pat', exp') else raise Goto end) match, comp), true)
      handle Goto => (FN_EXP (match, comp), false) end in
    ((exp', ts), fin)  end 

  and fillDec dec = (case dec of
      VAL_DEC valbind => let
      val (valbind', fin) = fillValbd valbind in
      (VAL_DEC valbind', fin) end
    | DAT_DEC datbind => (DAT_DEC datbind, true)
    | EXC_DEC exbind  => (EXC_DEC exbind, true)
    | LOC_DEC (dec1, dec2) => raise Match
    | SEQ_DEC decs => (SEQ_DEC (List.map (fn dec => let 
      val (dec', fin) = fillDec dec in 
      if fin then dec' else raise Goto end) decs), true)
      handle Goto => (dec, false))

  and fillValbd valbd = case valbd of
      NRE_VALBIND vrow => let
      val (vrow', fin) = fillVrow vrow in
      (NRE_VALBIND vrow', fin) end
    | REC_VALBIND vrow => let
      val (vrow', fin) = fillVrow vrow in
      (REC_VALBIND vrow', fin) end

  and fillVrow (vrow, comp) = ((List.map (fn (pat, exp) => let
    val (pat', fin1) = fillPat [#2 exp] pat
    val (exp', fin2) = fillExp [#2 pat] exp in
    if fin1 andalso fin2 then (pat', exp') else raise Goto end) vrow, comp), true)
    handle Goto => ((vrow, comp), false)

  and fillAtpat tss (atpat, ts) = let
    val (atpat', fin) = case atpat of
      WILD_ATPAT   => (WILD_ATPAT, true)
    | SCON_ATPAT s => (SCON_ATPAT s, true)
    | LVID_ATPAT l => (LVID_ATPAT l, true)
    | RCD_ATPAT patrow => (let 
      val patrow' = List.map (fn (lab, pat) => let
        val tss' = List.foldl (fn ((c, ROWTY (rty, w)),tss') =>
          (((c, valOf (LM.find (rty, lab))) :: tss')
            handle Option => tss') 
          | (_,tss') => tss') [] (ts :: tss) 
        val (pat', fin) = fillPat tss' pat in 
        if fin then (lab, pat') else raise Goto end) 
      val (addlabs, valid) = List.foldl (fn ((c, ROWTY (rty, false)), (res, t)) => let
        val res = LS.difference (LM.keySet rty,
          LS.fromList (List.map (fn (lab, exp) => lab) patrow)) in
          (res, true) end
        | (_, r) => r) (LS.empty, false) (ts :: tss) in
      if valid then (RCD_ATPAT (LS.foldl (fn (lab, pr) => pr @ [(lab, 
        (AT_PAT (WILD_ATPAT, tswild), tswild))]) patrow addlabs), true 
      )        else (atpat, false) end handle Goto => (atpat, false)) 
    | PAT_ATPAT pat => let 
      val (pat', fin) =  fillPat (ts :: tss) pat in
      (PAT_ATPAT pat', fin) end in 
    ((atpat', ts), fin) end

  and fillPat tss (pat, ts) = let
    val (pat', fin) = case pat of
      AT_PAT atpat => let
      val (atpat', fin) =  (fillAtpat (ts :: tss) atpat) in
      (AT_PAT atpat', fin) end
    | CON_PAT ((l, tsl), atpat) => let
      val (c, FUNTY (t1, t2)) = tsl
      val (atpat', fin) = fillAtpat [(c, t1)] atpat in
      (CON_PAT ((l,tsl), atpat'), fin) end
    | LAY_PAT _ => raise Match in 
    ((pat', ts), fin)  end

  and fillProg prog = let 
    val (ist, fin) = fillDec prog in 
    if fin then ist else raise Match end

  fun insAtexp imap (atexp, ts) = let
    val ts' = TS.instantiate ts imap in (case atexp of
      SCON_ATEXP s => (SCON_ATEXP s)
    | LVID_ATEXP l => (LVID_ATEXP l)
    | RCD_ATEXP r  => (RCD_ATEXP 
      (List.map (fn (lab, exp) => (lab, insExp imap exp)) r))
    | LET_ATEXP (dec, exp) => (LET_ATEXP (insDec imap dec, insExp imap exp))
    | EXP_ATEXP exp => (EXP_ATEXP (insExp imap exp)), ts') end
   
  and insExp imap (exp, ts) = (case exp of 
      AT_EXP  atexp => AT_EXP (insAtexp imap atexp)
    | APP_EXP (exp, atexp) => APP_EXP (insExp imap exp, insAtexp imap atexp)
    | HAND_EXP (exp, match) => HAND_EXP (insExp imap exp, insMatch imap match)
    | RAS_EXP exp => (RAS_EXP (insExp imap exp))
    | FN_EXP match => FN_EXP (insMatch imap match), 
    TS.instantiate ts imap)

  and insMatch imap (match, comp) = (List.map (fn (pat, exp) => 
    (insPat imap pat, insExp imap exp)) match, comp)

  and insAtpat imap (atpat, ts) = (case atpat of 
      WILD_ATPAT => WILD_ATPAT
    | SCON_ATPAT s => SCON_ATPAT s
    | LVID_ATPAT l => LVID_ATPAT l
    | RCD_ATPAT r => RCD_ATPAT 
      (List.map (fn (lab, pat) => (lab, insPat imap pat)) r)
    | PAT_ATPAT pat => PAT_ATPAT (insPat imap pat) , 
    TS.instantiate ts imap)

  and insPat imap (pat, ts) = (case pat of
      AT_PAT atpat => AT_PAT (insAtpat imap atpat)
    | CON_PAT ((l, tsl), atpat) => CON_PAT (
      (l, TS.instantiate tsl imap), insAtpat imap atpat)
    | LAY_PAT _ => raise Match,
    TS.instantiate ts imap)

  and insDec imap dec = case dec of
        VAL_DEC valbind => VAL_DEC (case valbind of
          NRE_VALBIND vrow => NRE_VALBIND (insVrow imap vrow)
        | REC_VALBIND vrow => REC_VALBIND (insVrow imap vrow))
      | DAT_DEC datbind => DAT_DEC datbind
      | EXC_DEC exbind  => EXC_DEC exbind
      | LOC_DEC (dec1, dec2) => LOC_DEC (insDec imap dec1, insDec imap dec2)
      | SEQ_DEC decs => SEQ_DEC (List.map (fn dec => insDec imap dec) decs)
  and insVrow imap (vrow, comp) = (List.map (fn (pat, exp) => 
    (insPat imap pat, insExp imap exp)) vrow, comp)

  val toString = progToString


end
