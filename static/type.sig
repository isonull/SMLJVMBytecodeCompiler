signature TYPE = sig

  type tyvar

  type tyname
  type rowty
  type funty
  type conty
  type tyfcn
  type tysch
  type tynameset
  type tyvarset
  type asstyset
  type assty
  datatype ty =
    VARTY of tyvar |
    ROWTY of rowty |
    FUNTY of funty |
    CONTY of conty |
    ASSTY of assty

  exception UnificationFail
  exception WrongTypeForm

  (* l stands for the first sub/ins is at the head of the list *)
  (* r stands for the first sub/ins is at the last of the list *)
  (* l version is preferred, because of tail recursion *)
  type sub = ty * ty
  type subsr = sub list
  type subsl = sub list
  type ins = ty * ty
  type inssr = ins list
  type inssl = ins list

  val emptyT : tynameset
  val emptyU : tyvarset
  val emptyA : asstyset

  val remVartyCon : ty -> Identifier.tyvar
  val remAsstyCon : ty -> assty

  val modT : tynameset -> tynameset -> tynameset
  val modU : tyvarset -> tyvarset -> tyvarset
  val modA : asstyset -> asstyset -> asstyset

  val getTyvarsInTy : ty -> tyvarset
  val getUinTysch : tysch -> tyvarset

  (* return the modified first tysch input disjoining the second tysch *)
  val disjoinTysch : tysch -> tysch -> tysch * tysch

  val tynameInTy : tyname -> ty
  val tynameInTysch : tyname -> tysch
  val tynameInTyfcn : tyname -> tysch

  val getArgTyschInFunTysch : tysch -> tysch
  val getResTyschInFunTysch : tysch -> tysch
  val splitFunTysch : tysch -> tysch * tysch

  val tySub : ty -> sub -> ty
  val tySubsr : ty -> subsr -> ty
  val tySubsl : ty -> subsl -> ty

  (* rebind only changes the name of closed tyvars *)
  (* the third argument is for introducing new possible tyvars *)
  val tyschRebindr : tysch -> subsr -> tyvar list -> tysch
  val tyschRebindl : tysch -> subsl -> tyvar list -> tysch

  (* substitution only changes the name of opened tyvars *)
  val tyschSubsl : tysch -> subsl -> tyvar list-> tysch
  val tyschInssl : tysch -> inssl -> tyvar list -> tysch

  val regTysch : tysch -> tysch

  val applyTyfcn : tyfcn -> ty list -> ty

  val genTyUniSubsr : SBS.set -> ty -> ty
                        -> (subsr * inssr) -> bool -> (subsr * inssr)
  val genTyUniSubsl : SBS.set -> ty -> ty
                        -> (subsl * inssl) -> bool -> (subsl * inssl)

  (* the tyvarset contains free tyvars *)
  val genTyschUniSubsl : tysch -> tysch -> (subsl * inssl) * tyvar list

  val genDisjoinSubsl : tyvar list -> SBS.set -> subsl

  val tynameToString : tyname -> string
  val tyschToString : tysch -> string
  val tyfcnToString : tyfcn -> string
  val tyToString : ty -> string

end
