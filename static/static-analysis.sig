signature STATIC_ANALYSIS = sig

  val infScon    : CST.scon            -> Type.tysch
  val infAtexp   : StaticBasis.context -> CST.atexp   -> StaticBasis.context * Type.tysch
  val infExprow  : StaticBasis.context -> CST.atexp   -> StaticBasis.context * Type.tysch
  val infExp     : StaticBasis.context -> CST.exp     -> StaticBasis.context * Type.tysch
  val infMatch   : StaticBasis.context -> CST.match   -> StaticBasis.context * Type.tysch
  val infMrule   : StaticBasis.context -> CST.mrule   -> StaticBasis.context * Type.tysch
  val infDec     : StaticBasis.context -> CST.dec     -> StaticBasis.context * Type.tysch
  val infValbind : StaticBasis.context -> CST.valbind -> StaticBasis.context * Type.tysch
  val infTypbind : StaticBasis.context -> CST.typbind -> Type.tysch
  val infDatbind : StaticBasis.context -> CST.datbind -> Type.tysch
  val infConbind : StaticBasis.context -> CST.conbind -> Type.tysch
  val infExbind  : StaticBasis.context -> CST.exbind  -> Type.tysch
  val infAtpat   : StaticBasis.context -> CST.atpat   -> Type.tysch
  val infPatrow  : StaticBasis.context -> CST.patrow  -> Type.tysch
  val infPat     : StaticBasis.context -> CST.pat     -> Type.tysch
  val infTy      : StaticBasis.context -> CST.ty      -> Type.tysch
  val infTyrow   : StaticBasis.context -> CST.tyrow   -> Type.tysch

end
