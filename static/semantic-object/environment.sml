structure Environment = struct
  structure SE = StructureEnvironment
  structure TE = TypeEnvironment
  structure VE = ValueEnvironment
  structure SM = StringBinaryMap
  structure SID = StructureEnvironment

  datatype env = datatype SE.env

  val empty = ENV (SE.empty, TE.empty, VE.empty)

  fun instantiate (ENV (se, te, ve)) is = ENV (se, te, VE.instantiate ve is)

  fun getAsstyset (ENV (se, te, ve)) = VE.getAsstyset ve

  fun getValstr (ENV (se, _, ve)) (sid :: pre, vid) = let
    val envOp = SE.find (se, sid) in
    Option.join (Option.map (fn e => getValstr e (pre, vid)) envOp) end
    | getValstr (ENV (_, _, ve)) ([], vid) = VE.find (ve, vid)

  fun getTystr (ENV (se, te, _)) (sid :: pre, tycon) = let
    val envOp = SE.find (se, sid) in
    Option.join (Option.map (fn e => getTystr e (pre, tycon)) envOp) end
    | getTystr (ENV (_, te, _)) ([], tycon) = TE.find (te, tycon)

  fun strenvAugment (ENV (se, te, ve)) se' = ENV (SE.unionWith #2 (se, se'), te, ve)
  fun tyenvAugment (ENV (se, te, ve)) te' = ENV (se, TE.unionWith #2 (te, te'), ve)
  fun valenvAugment (ENV (se, te, ve)) ve' = ENV (se, te, VE.unionWith #2 (ve, ve'))
  fun modify (ENV (se, te, ve)) (ENV (se', te', ve')) = ENV (
    SE.unionWith #2 (se, se'),
    TE.unionWith #2 (te, te'),
    VE.unionWith #2 (ve, ve'))

  val getTynameset = SE.getTynamesetEnv

  fun fromValenv ve = ENV (SE.empty, TE.empty, ve)
  fun fromTyenv te = ENV (SE.empty, te, VE.empty)

  fun toString (ENV (se, te, ve)) =
    (SE.toString se) ^ "\n\n" ^
    (TE.toString te) ^ "\n\n" ^
    (VE.toString ve) ^ "\n\n"
end
