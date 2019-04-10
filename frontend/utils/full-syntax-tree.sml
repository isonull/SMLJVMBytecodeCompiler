structure FullSyntaxTree = struct

  structure SC = SpecialConstant
  datatype lab = datatype Lab.lab
  datatype scon = datatype SC.scon

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

  withtype id = string
  and tyvar = string * bool (* equalable *) 

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

  fun lidById id = ([], id)
  fun expById id = INF_EXP(APP_INFEXP([LVID_ATEXP (lidById id)]))
  fun patById id = AT_PAT (LVID_ATPAT (lidById id))

  val wildPat = AT_PAT WILD_ATPAT
  val unitExp = INF_EXP(APP_INFEXP([UNIT_ATEXP]))
  val unitPat = AT_PAT(UNIT_ATPAT)
  fun expByAppexp e = INF_EXP (APP_INFEXP e)
  fun expByAtexp e = expByAppexp [e]
  fun atexpByAppexp e = EXP_ATEXP (expByAppexp e)

  local val count = ref 0 in
  fun getUniqueId () = let
    val preservedPrefix = "_"
  in
    count := (!count) + 1;
    preservedPrefix ^ (Int.toString (!count))
  end end

  fun getUniqueIdseq 0 = nil
    | getUniqueIdseq n =
    if n < 0
    then raise DerivedFormException
    else (getUniqueId ()) :: (getUniqueIdseq (n - 1))


  fun unitAtexpToRcdAtexp UNIT_ATEXP = RCD_ATEXP nil
    | unitAtexpToRcdAtexp _ = raise DerivedFormException

  fun tupAtexpToRcdAtexp (TUP_ATEXP es) = let
    fun aux (e :: es) i = (INT_LAB i, e) :: (aux es (i + 1))
      | aux nil _ = nil
  in
    if length es < 2
    then raise DerivedFormException
    else RCD_ATEXP (aux es 1)
  end
    | tupAtexpToRcdAtexp _ = raise DerivedFormException

  (* TODO new id*)
  fun labAtexpToFnExp (LAB_ATEXP l) = let
    val exp = expById "x"
    val pat    = patById "x"
    val patrow = [LAB_PATROW_ELE (l, pat), WILD_PATROW_ELE]
  in
    FN_EXP [(AT_PAT (RCD_ATPAT patrow), exp)]
  end
    | labAtexpToFnExp _ = raise DerivedFormException

  fun caseExpToAppexp (CASE_EXP (e, m)) =
    [EXP_ATEXP e, EXP_ATEXP (FN_EXP m)]
    | caseExpToAppexp _ = raise DerivedFormException

  fun ifExpToCaseExp (IF_EXP (e, e', e'')) = let
    val match = [(patById "true", e'),(patById "false", e'')]
  in
    CASE_EXP (e, match)
  end
    | ifExpToCaseExp _ = raise DerivedFormException

  fun orExpToIfExp (OR_EXP (e, e')) = IF_EXP(e, expById "true", e')
    | orExpToIfExp _ = raise DerivedFormException

  fun andExpToIfExp (AND_EXP (e, e')) = IF_EXP(e, expById "false", e')
    | andExpToIfExp _ = raise DerivedFormException

  fun seqAtexpToCaseExp (SEQ_ATEXP (e :: nil)) = e
    | seqAtexpToCaseExp (SEQ_ATEXP (e :: es)) =
    CASE_EXP (e, [(wildPat, seqAtexpToCaseExp (SEQ_ATEXP es))])
    | seqAtexpToCaseExp (SEQ_ATEXP nil) = raise DerivedFormException
    | seqAtexpToCaseExp _ = raise DerivedFormException

  (* TODO new id*)
  fun whilExpToLetAtexp (WHIL_EXP (e, e')) = let
    val id = getUniqueId ()
    val brTrue = expByAtexp (SEQ_ATEXP [e', 
      expByAppexp ([UNIT_ATEXP, LVID_ATEXP (lidById id)])])
    val ifExp = IF_EXP (e, brTrue, unitExp)
    val fnExp = FN_EXP [(unitPat, ifExp)]
    val valbind = REC_VALBIND [(patById id, fnExp)]
    val letBody = expByAppexp [UNIT_ATEXP, LVID_ATEXP (lidById id)]
  in
    LET_ATEXP (VAL_DEC ([],valbind), [letBody])
  end
    | whilExpToLetAtexp _ = raise DerivedFormException

  fun listAtexpToInfexp (LIST_ATEXP (e :: es)) =
    N_INFEXP (APP_INFEXP [EXP_ATEXP e], "::", listAtexpToInfexp (LIST_ATEXP es))
    | listAtexpToInfexp (LIST_ATEXP nil) = APP_INFEXP [LVID_ATEXP (lidById "nil")]
    | listAtexpToInfexp _ = raise DerivedFormException

  fun unitAtpatToRcdAtpat UNIT_ATPAT = RCD_ATPAT nil
    | unitAtpatToRcdAtpat _ = raise DerivedFormException

  fun tupAtpatToRcdAtpat (TUP_ATPAT ps) = let
    fun aux (p :: ps) i = LAB_PATROW_ELE (INT_LAB i, p) :: (aux ps (i + 1))
      | aux nil _ = nil
  in
    if length ps < 2
    then raise DerivedFormException
    else RCD_ATPAT (aux ps 1)
  end
    | tupAtpatToRcdAtpat _ = raise DerivedFormException

  fun listAtpatToInfPat (LIST_ATPAT (p :: ps)) =
    INF_PAT (p, "::", listAtpatToInfPat (LIST_ATPAT ps))
    | listAtpatToInfPat (LIST_ATPAT nil) =
    AT_PAT (LVID_ATPAT (lidById "nil"))
    | listAtpatToInfPat _ = raise DerivedFormException

  fun vidPatroweleToLabPatrowele (VID_PATROW_ELE (vid, a, SOME b)) =
    LAB_PATROW_ELE (STR_LAB vid, LAY_PAT (vid, a, b))
    | vidPatroweleToLabPatrowele (VID_PATROW_ELE (vid, a, NONE)) =
    LAB_PATROW_ELE (STR_LAB vid, patById vid)
    | vidPatroweleToLabPatrowele _ = raise DerivedFormException

  fun tupTyToRcdTy (TUP_TY ts) = let
    fun aux (t :: ts) i = (INT_LAB i, t) :: (aux ts (i + 1))
      | aux nil _ = nil
  in
    if length ts < 2
    then raise DerivedFormException
    else RCD_TY (aux ts 1)
  end
    | tupTyToRcdTy _ = raise DerivedFormException

  fun frowToAtpat nil = raise DerivedFormException
    | frowToAtpat (p :: nil) = p
    | frowToAtpat ps = TUP_ATPAT (map AT_PAT ps)

  fun fvaleleToMrule (_, frow, exp) = (AT_PAT (frowToAtpat frow), exp)

  fun fvalToVrowele rs = let
    (* optimisation from n to log n*)
    fun fvalGetVidArgNum (((vid : string), frow, _) :: (vid', frow', exp') :: rs) =
      if (vid = vid') andalso (length frow = length frow')
      then fvalGetVidArgNum ((vid', frow', exp') :: rs)
      else raise DerivedFormException
      | fvalGetVidArgNum ((vid, frow, _) :: nil) = (vid, length frow)
      | fvalGetVidArgNum nil = raise DerivedFormException

    val (vid, argNum) = fvalGetVidArgNum rs

    val match = map fvaleleToMrule rs
    val idseq = getUniqueIdseq argNum

    fun getCase 1 = let
          val lvidExp = expById (List.nth (idseq, 0))
      in CASE_EXP (lvidExp, match) end
      | getCase _ = let
          val tupAtexp = TUP_ATEXP (map expById idseq)
      in CASE_EXP (expByAtexp tupAtexp, match) end

    fun getFns 0 _ = getCase argNum
      | getFns n i =
      FN_EXP [(patById (List.nth (idseq, i)), getFns (n - 1) (i + 1))]
  in
    (print ("test" ^ Int.toString argNum);(patById vid, getFns argNum 0))
  end

  fun fvalbindToValbind nil = raise DerivedFormException
    | fvalbindToValbind fs = REC_VALBIND (map fvalToVrowele fs)

  fun funDecToValDec (FUN_DEC (tyvarseq, fvalbind)) =
    VAL_DEC (tyvarseq, fvalbindToValbind fvalbind)
    | funDecToValDec _ = raise DerivedFormException

  (* TODO substitution sml97-dfn p67*)

  fun separateDatbindTypbind datbind typbind = datbind

  fun datDecReduce (DAT_DEC (datbind, SOME (typbind))) =
    SEQ_DEC (DAT_DEC (separateDatbindTypbind datbind typbind, NONE),
             TYP_DEC typbind)
    | datDecReduce (DAT_DEC a) =  DAT_DEC a
    | datDecReduce _ = raise DerivedFormException

  fun absDecReduce (ABS_DEC (datbind, SOME typbind, dec)) =
    ABS_DEC (separateDatbindTypbind datbind typbind, NONE,
      SEQ_DEC (TYP_DEC typbind ,dec))
    | absDecReduce (ABS_DEC d) = ABS_DEC d
    | absDecReduce _ = raise DerivedFormException

  val ifExpToAppexp = caseExpToAppexp o ifExpToCaseExp
  val andExpToAppexp = ifExpToAppexp o andExpToIfExp
  val orExpToAppexp = ifExpToAppexp o orExpToIfExp
  val seqAtexpToAppexp = caseExpToAppexp o seqAtexpToCaseExp

end

structure FST = FullSyntaxTree
