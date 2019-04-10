structure TypeEnvironment = struct

  structure TS = TypeStructure
  structure TF = TypeFunction
  structure ID = Identifier
  structure TYC = TypeConstructor
  structure SM = StringBinaryMap
  structure T = TypeNameSet
  structure LM = LabBinaryMap

  open StringBinaryMap

  type tyenv = TS.tystr map

  fun getTynameset te = foldli (fn (tc, (tf, ve), set) =>
    T.add (set, (([], tc), TF.getArity tf, false))) T.empty te

  fun getVidTyfcnMap te = foldli (fn (tc, (tf, _), map) => 
    SM.insert (map, tc, tf)) SM.empty te

  fun groundTysch te (c, ty) = let
    datatype ty = datatype Type.ty
    
    fun recCheck (CONTY (ts, tn)) (CONTY (ts', tn')) = TypeName.equal tn tn' 
      | recCheck _ _ = false

    fun groundTy (VARTY i) = VARTY i 
      | groundTy (ASSTY i) = ASSTY i 
      | groundTy (ROWTY (r, w)) = ROWTY (LM.map (fn t => groundTy t) r, w)
      | groundTy (CONTY (ts, t as (([], tn), _, _))) = (let 
      val (tf, _) = Option.valOf (find (te, tn))
      val tss = List.map (fn ty => (c, ty)) ts
      val (_, ty) = TF.apply tss tf in 
        if recCheck ty (CONTY (ts, t)) then 
          (CONTY (ts, t)) else groundTy ty
      end handle Option => (CONTY (ts, t)))
      | groundTy (FUNTY (a, b)) = FUNTY (groundTy a, groundTy b)
  in TypeScheme.reg (c, groundTy ty) end

  fun toString te =
    SM.toString te TYC.toString TS.toString " - " "\n"
end
