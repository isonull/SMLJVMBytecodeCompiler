structure DynamicInference = struct

  structure CST = CoreSyntaxTree
  structure S = Space
  structure VS = ValueSpace
  structure TS = TypeSpace
  structure SS = StructureSpace
  structure LM = LabBinaryMap
  structure IS = IdentifierStatus

  structure VIS = ValueIndexSet

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
  datatype lab       = datatype Lab.lab

  datatype instruction = datatype Slang.instruction
  datatype const = datatype Slang.const

  fun update r f = r := f (! r)

  val top = ref 0
  val fname = ref "a"
  val flistRef = ref ([] : ((string * (Slang.instruction list)) list))

  fun getTop () = let
    val r = ! top in 
    (top := !top + 1; r) end

  fun getFname () = let
    val r = ! fname in
    (fname := StringAux.succ (! fname); r) end

  fun infScon (INT_SCON  i) = [CONST (I i)] 
    | infScon (REAL_SCON r) = [CONST (R r)]
    | infScon _ = raise Size

  and infAtexp spa (SCON_ATEXP scon) = infScon scon
    | infAtexp spa (LVID_ATEXP lvid) = let
    val valstr = Option.valOf (S.getValstr spa lvid)
    val code = case valstr of
      (id, VAL) => [GET id]
    | (id, CON) => [GET id]
    | (id, EXC) => raise Size in code end
    | infAtexp spa (EXP_ATEXP exp) = infExp spa exp
    | infAtexp spa (RCD_ATEXP exprow) =
    NEWRCD :: (infExprow spa exprow)

  and infExprow spa exprow = let
    val maxLabi = ref (~1)
    fun aux (lab, exp) = let
      val expCode = infExp spa exp
      val iCode   = [CONST (S (case lab of
        INT_LAB i => Int.toString i
      | STR_LAB s => s))] in
      [DUPL] @ iCode @ expCode @ [PUTRCD, REMO] end in
    (List.concat o (List.map aux)) exprow end

  and infExp spa (AT_EXP atexp) = infAtexp spa atexp
    | infExp spa (FN_EXP match) = let
    val fname = getFname ()
    val codeMatch = infMatch spa match 
    val codeFn = (LOAD 0) :: codeMatch @ [RETURN] in
    flistRef := (! flistRef) @ [(fname, codeFn)];
    [GETF ((length (! flistRef)) - 1)] end
    | infExp spa (APP_EXP (exp, atexp)) = let
    val expCode = infExp spa exp
    val atexpCode = infAtexp spa atexp in
    expCode @ atexpCode @ [CALL] end

  and infMrule spa (pat, exp) = let
    val (vsPat, codePat) = infPat spa pat
    val spaMod = S.modify spa (S.fromValspa vsPat)
    val codeExp = infExp spaMod exp in
    codePat @ codeExp end

  and infMatch spa [mrule] = infMrule spa mrule
    | infMatch spa _ = raise Size

  and infDec spa (VAL_DEC (_, valbd)) = let
    val (vs, code) = infValbind spa valbd
    val s = S.fromValspa vs in (s, code) end

    | infDec spa (SEQ_DEC (dec1, dec2)) = let
    val (spa1, code1) = infDec spa dec1
    val spaMod = S.modify spa spa1
    val (spa2, code2) = infDec spaMod dec2 
    val s = S.modify spa1 spa2 in
    (s, code1 @ code2) end

    | infDec spa (DAT_DEC (datbind)) = let
    val (vs, ts, code) = infDatbind datbind
    val s = Value.SPA (SS.empty, ts, vs) in
    (s, code) end

  and infValbind spa (NRE_VALBIND (vrow)) = 
    infVrow spa vrow
    | infValbind spa (REC_VALBIND (vrow)) = let
    val (vs, patCodes) = introVrow spa vrow
    val recspa = S.modifyValspa spa vs
    val expCodes = map (fn (_, exp) => infExp recspa exp) vrow
    val code = List.concat (List.map (fn (c1, c2) => c1 @ c2) 
      (ListPair.zip (expCodes, patCodes))) in
    (vs, code) end

  and introVrow spa ((pat, _) :: vs) = let
    val (patVs, patCode) = infPat spa pat
    val (vsVs, vsCodes) = introVrow spa vs
    val vs = VS.modify patVs vsVs
    val codes = patCode :: vsCodes in
    (vs, codes) end
    | introVrow spa [] = (VS.empty, [])

  and infVrow spa [v] = let
    val (vs, code) = infVrowele spa v in
    (vs, code) end
    | infVrow spa (v :: vs) = let
    val (vsV, codeV) = infVrowele spa v
    val (vsVs, codeVs) = infVrow spa vs
    val vs = VS.modify vsV vsVs
    val code = codeV @ codeVs in
    (vs, code) end
    | infVrow spa [] = raise Size

  and infVrowele spa (pat, exp) = let
    val (vsPat, codePat) = infPat spa pat
    val codeExp = infExp spa exp in
    (vsPat, codeExp @ codePat) end

  and infDatbind cons = let
    val (vs, ts, code) = List.foldl 
    (fn ((_, tc, con), (vs, ts, cd)) => let
      val (v, c) = infConbind con
      val nvs = VS.modify vs v
      val ncd = cd @ c
      val t   = TS.fromListPair [(tc, v)] 
      val nts = TS.modify ts t in
      (nvs, nts, ncd) end) (VS.empty, TS.empty, []) cons in
    (vs, ts, code) end

  and infConbind conbd = let
    fun faux vid = let
      val id = getTop ()
      val vs = VS.fromListPair [(vid, (id, IS.CON))]
      val code = [NEWCON, DUPL, CONST (I id), LOAD 0, PUTCON, RETURN] in
      flistRef := (! flistRef) @ [(vid ^ (Int.toString id), code)];
      (vs, [GETF ((length (! flistRef)) - 1), PUT id]) end

    fun vaux vid = let
      val id = getTop ()
      val vs = VS.fromListPair [(vid, (id, IS.CON))] in
      (vs, [CONST (I id), PUT id]) end

    fun aux (vid, SOME _) = faux vid
      | aux (vid, NONE)   = vaux vid

    val (vs, code) = List.foldl (fn (con, (vs, code)) => let
      val (v, c) = aux con
      val nvs    = VS.modify vs v
      val ncode  = code @ c in
      (nvs, ncode) end) (VS.empty, []) conbd in
    (vs, code) end


  and infAtpat spa (LVID_ATPAT ([], vid)) = let
    val id = getTop ()
    val vs = VS.fromListPair [(vid, (id, IS.VAL))] in
    (vs, [PUT id]) end
    | infAtpat _ _ = raise Size

  and infPat spa (AT_PAT atpat) = infAtpat spa atpat
    | infPat spa _ = raise Size

  and infStrdec _ = raise Size

  and infProg spa prog = let 
    val (spaProg, initCode) = infDec spa prog
    val fields   = [] in
    (! flistRef, initCode, spaProg) end

  fun inference prog = (
    top := 0;
    flistRef := [];
    infProg S.empty prog)

end
