structure ConstantPool = struct

  datatype name = datatype Name.name
  datatype descriptor = datatype Descriptor.descriptor
  datatype refkind = datatype ReferenceKind.refkind
  datatype cst = datatype Constant.constant
  datatype utf = datatype Constant.utf
  datatype refkind = datatype ReferenceKind.refkind

  datatype constant =
    C_CLASS    of string list |
    C_FREF     of string list * string * descriptor |
    C_MREF     of string list * string * descriptor |
    C_IREF     of string list * string * descriptor |
    C_STR      of string |
    C_INT      of int |
    C_NAMETYPE of string * descriptor |
    C_MHANDLE  of refkind * (string list * string * descriptor) |
    C_UTF      of utf

  val valOf = Option.valOf
  val isSome = Option.isSome

  fun find cp c = let

  fun find1 c = ListAux.findIndexFrom cp c 1

  fun faux (cn, n, d) = let
    val idCn = aux (C_UTF (UTF_NAME (NBIN cn)))
    val idNt = aux (C_NAMETYPE (n, d)) in
    (valOf idCn, valOf idNt) end

  and aux (C_CLASS   cn) = let
    val idCn = aux (C_UTF (UTF_NAME (NBIN cn))) in
    find1 (CST_CLASS (valOf idCn)) end

    | aux (C_FREF    (cn, n, d)) =   
    find1 (CST_FREF (faux (cn, n, d)))

    | aux (C_MREF    (cn, n, d)) =
    find1 (CST_MREF (faux (cn, n, d)))

    | aux (C_IREF    (cn, n, d)) =
    find1 (CST_IREF (faux (cn, n, d)))

    | aux (C_STR     s) = let
    val idS = aux (C_UTF (UTF_STRI s)) in
    find1 (CST_STR (valOf idS)) end

    | aux (C_INT     i) =
    find1 (CST_INT i)

    | aux (C_NAMETYPE (n, d)) = let
    val idN = aux (C_UTF (UTF_NAME (NUNQ n)))
    val idD = aux (C_UTF (UTF_DESC d)) in
    find1 (CST_NAMETYPE (valOf idN, valOf idD)) end

    | aux (C_MHANDLE (rk, r)) = let
    val idD = aux (case rk of 
      REF_GETF   => C_FREF (r)
    | REF_GETS   => C_FREF (r)                 
    | REF_PUTF   => C_FREF (r)                 
    | REF_PUTS   => C_FREF (r)                 
    | REF_INVVI  => C_MREF (r)                 
    | REF_INVST  => C_MREF (r)                 
    | REF_INVSP  => C_MREF (r)                 
    | REF_NINVSP => C_MREF (r)                 
    | REF_INVINT => C_IREF (r)
    ) in
    find1 (CST_MHAND (rk, valOf idD)) end

    | aux (C_UTF     u) =
    find1 (CST_UTF8 u) in

    aux c handle Option => NONE end

  fun add cp c = let
    val id = find cp c in 
    if isSome id then (cp, valOf id) 

    else let val newcp = (
    case c of
      (C_CLASS cn) => let
    val (newcp, id) = add cp (C_UTF (UTF_NAME (NBIN cn))) in
    newcp @ [CST_CLASS id] end

    | (C_FREF (cn, mn, dc)) => let
    val (newcp1, id1) = add cp (C_CLASS cn)
    val (newcp2, id2) = add newcp1 (C_NAMETYPE (mn, dc)) in
    newcp2 @ [CST_FREF (id1, id2) ] end

    | (C_MREF   (cn, mn, dc)) => let
    val (newcp1, id1) = add cp (C_CLASS cn)
    val (newcp2, id2) = add newcp1 (C_NAMETYPE (mn, dc)) in
    (newcp2 @ [CST_MREF (id1, id2)]) end

    | (C_INT i) => (cp @ [CST_INT i])

    | (C_NAMETYPE (n, d)) => let
    val (newcp1, id1) = add cp (C_UTF (UTF_NAME (NUNQ n)))
    val (newcp2, id2) = add newcp1 (C_UTF (UTF_DESC d)) in
    (newcp2 @ [CST_NAMETYPE (id1, id2)]) end

    | (C_MHANDLE (rk, r)) => let
    val (newcp1, id1) = add cp (case rk of
      REF_GETF   => C_FREF (r)
    | REF_GETS   => C_FREF (r)                 
    | REF_PUTF   => C_FREF (r)                 
    | REF_PUTS   => C_FREF (r)                 
    | REF_INVVI  => C_MREF (r)                 
    | REF_INVST  => C_MREF (r)                 
    | REF_INVSP  => C_MREF (r)                 
    | REF_NINVSP => C_MREF (r)                 
    | REF_INVINT => C_IREF (r)) in 
    (newcp1 @ [CST_MHAND (rk, id1)]) end

    | C_UTF utf => 
    (cp @ [CST_UTF8 utf])

    | C_STR s => let
    val (newcp1, id1) = add cp  (C_UTF (UTF_STRI s)) in
    (newcp1 @ [CST_STR id1]) end
    ) in (newcp, length newcp) end end

  fun radd cpref c = let
    val (newcp, id) = add (! cpref) c in (
    cpref := newcp; id) end

end
