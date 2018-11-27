signature STATIC_BASIS = sig

  datatype idstat = VAL | CON | EXC

  type valstr
  type tystr

  type valenv
  type tyenv
  type strenv
  datatype env = ENV of strenv * tyenv * valenv

  type context

  exception ContextSearchFail
  val emptySE : strenv
  val emptyTE : tyenv
  val emptyVE : valenv

  val emptyE : env
  val emptyC : context

  val EbySE : strenv -> env
  val EbyTE : tyenv -> env
  val EbyVE : valenv -> env

  val CbyT : Type.tynameset -> context
  val CbyU : Type.tyvarset -> context
  val CbyA : Type.asstyset -> context
  val CbyE : env -> context

  val SEofE : env -> strenv
  val TEofE : env -> tyenv
  val VEofE : env -> valenv

  val UofC : context -> Type.tyvarset
  val TofC : context -> Type.tynameset
  val AofC : context -> Type.asstyset
  val EofC : context -> env

  val modSE : strenv -> strenv -> strenv
  val modVE : valenv -> valenv -> valenv
  val modTE : tyenv -> tyenv -> tyenv

  val modE : env -> env -> env
  val modC : context -> context -> context

  val mapT : (Type.tynameset -> Type.tynameset ) -> context -> context
  val mapU : (Type.tyvarset  -> Type.tyvarset  ) -> context -> context
  val mapA : (Type.asstyset  -> Type.asstyset  ) -> context -> context
  val mapE : (env       -> env       ) -> context -> context

  val mapSE : (strenv -> strenv) -> env -> env
  val mapTE : (tyenv  -> tyenv ) -> env -> env
  val mapVE : (valenv -> valenv) -> env -> env

  val getVSinE : env -> Identifier.vid -> valstr option
  val getTSinE : env -> Identifier.tycon -> tystr option
  val getEinE  : env -> Identifier.sid -> env option

  val pgetEinE : env -> Identifier.lidpre -> env option

  val lgetVSinE : env -> Identifier.lvid -> valstr option
  val lgetTSinE : env -> Identifier.ltycon -> tystr option
  val lgetEinE  : env -> Identifier.lsid -> env option

  val lgetTSinC : context -> Identifier.ltycon -> tystr option
  val lgetVSinC : context -> Identifier.lvid -> valstr option
  val lgetEinC : context -> Identifier.lsid -> env option

  val getTinTE : tyenv -> Identifier.lidpre -> Type.tynameset
  val getTinE : env -> Type.tynameset

  val getUinVE : valenv -> Type.tyvarset
  (*val getUinE : env -> Type.tyvarset*)

  val expandAsstyTyschToFunAsstysch : context -> Type.tysch -> context * Type.tysch
  val insslC : context -> Type.inssl -> Identifier.tyvar list -> context

  val unifyTysch : context -> Type.tysch -> Type.tysch -> context * Type.tysch

  val contextToString : context -> string

end
