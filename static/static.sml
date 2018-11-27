structure StaticAnalysis = struct
  open Type
  open StaticBasis
  open CoreSyntaxTree

  structure Init = InitialStaticBasis

  exception TypeInferenceFail

  fun infScon (INT_SCON _) =  Type.tynameInTysch Init.intTyname
    | infScon (REAL_SCON _) = Type.tynameInTysch Init.realTyname
    | infScon (WORD_SCON _) = Type.tynameInTysch Init.wordTyname
    | infScon (CHAR_SCON _) = Type.tynameInTysch Init.charTyname
    | infScon (STR_SCON _)  = Type.tynameInTysch Init.strTyname

  fun infAtexp ctx (SCON_ATEXP scon) = infScon scon
    | infAtexp ctx (LVID_ATEXP lvid) = let
      val tyschIs = lgetVSinC ctx lvid in
        if Option.isSome tyschIs then #1 (Option.valOf tyschIs)
        else raise TypeInferenceFail end
    | infAtexp ctx (RCD_ATEXP exprow) =
    if length exprow = 0
    then Init.unitTysch
    else infExprow ctx exprow
    | infAtexp ctx (LET_ATEXP (dec, exp)) = let
      val augEnv = infDec ctx dec
      val expCtx = modC (modC ctx (CbyE augEnv)) (CbyT (getTinE augEnv))
    in infExp expCtx exp end
    | infAtexp _ _ = raise TypeInferenceFail

  and infExprow _ _ = Init.unitTysch

  (TODO: exception and handle case)
  and infExp ctx (AT_EXP atexp) = infAtexp ctx atexp
    | infExp ctx (APP_EXP (exp, atexp)) = let
      val expTy = infExp ctx exp
      val atexpTy = infAtexp ctx atexp
    in unifyTysch ctx (getArgTyschInFunTysch expTy) atexpTy end

    | infExp ctx (TY_EXP (exp, ty)) = let
      val expTy = infExp ctx exp
      val tyTy = infTy ctx ty
      
    in unifyTysch ctx tyTy expTy end
    
    | infExp ctx (FN_EXP match) = infMatch ctx (FN_EXP match)
    | infExp _ _ = raise TypeInferenceFail

  (* require unification transition property *)
  and infMatch ctx [r] = infMrule ctx r
    | infMatch ctx (r :: rs) = let
    val mruleTy = infMrule ctx r
    val matchTy = infMatch ctx rs
  in unifyTysch ctx mruleTy matchTy end

  and infMrule ctx (pat, exp) = let
    val (valenvPat, tyPat) = infPat ctx pat
    val expCtx = modC ctx (CbyE (EbyVE valenvPat))
    val (inssl, expTy) = infExp expCtx exp
    val tynamesetInValenv = getTinVE valenvPat
    (* instantiate the assumed type from exp inference *)
    val tyPat = tyschSubsl tyPat inssl
  in if TBS.isSubset (tynamesetInValenv, UofC ctx)
     then tyschPairToFunTysch tyPat expTy
     else raise TypeInferenceFail end

  and infDec ctx (VAL_DEC (tyvarseq, valbind)) = emptyE
    | infDec _ _ = raise TypeInferenceFail
end
