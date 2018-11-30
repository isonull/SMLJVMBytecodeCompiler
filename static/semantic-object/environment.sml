structure Environment = struct
  structure SE = StructureEnvironment
  structure TE = TypeEnvironment
  structure VE = ValueEnvironment
  structure SM = StringBinaryMap
  structure SID = StructureEnvironment

  datatype env = datatype SE.env

  fun instantiate (ENV (se, te, ve)) is = ENV (se, te, VE.instantiate ve is)

  fun getValstr (ENV (se, _, ve)) (sid :: pre, vid) = let
    val envOp = SE.find (se, sid) in
    Option.join (Option.map (fn e => getValstr e (pre, vid)) envOp) end
    | getValstr (ENV (_, _, ve)) ([], vid) = VE.find (ve, vid)

  fun getTystr (ENV (se, te, _)) (sid :: pre, tycon) = let
    val envOp = SE.find (se, sid) in
    Option.join (Option.map (fn e => getTystr e (pre, tycon)) envOp) end
    | getTystr (ENV (_, te, _)) ([], tycon) = TE.find (te, tycon)

  fun strenvAugment (ENV (se, te, ve)) se' = ENV (SE.intersectWith #2 (se, se'), te, ve)
  fun tyenvAugment (ENV (se, te, ve)) te' = ENV (se, TE.intersectWith #2 (te, te'), ve)
  fun valenvAugment (ENV (se, te, ve)) ve' = ENV (se, te, VE.intersectWith #2 (ve, ve'))

  val getTynameset = SE.getTynamesetEnv

  fun toString (ENV (se, te, ve)) =
    (SE.toString se) ^ "\n\n" ^
    (TE.toString te) ^ "\n\n" ^
    (VE.toString ve) ^ "\n\n"
end
