signature TYPE = sig

  type rowty
  type funty
  type conty
  type assty
  type varty

  type subseq
  type insseq
  type bndseq
  type vartyset

  datatype ty =
    VARTY of tyvar |
    ROWTY of rowty |
    FUNTY of funty |
    CONTY of conty |
    ASSTY of assty

  (* all exposed interface works on left sequence
   * such that the earlier substitution is at the list head*)
  val substitute : ty -> subseq -> ty
  val instantiate : ty -> insseq -> ty
  val bind : ty -> bndseq -> ty
  val unify : vartyset -> ty -> ty -> ty

end
