structure Constant = struct

  datatype name = datatype Name.name
  datatype descriptor = datatype Descriptor.descriptor
  datatype refkind = datatype ReferenceKind.refkind

  datatype constant =
    CST_CLASS    of cpindex |
    CST_FREF     of cpindex * cpindex |
    CST_MREF     of cpindex * cpindex |
    CST_IREF     of cpindex * cpindex |
    CST_STR      of cpindex |
    CST_INT      of int |
    CST_FLOAT    of string |
    CST_LONG     of int |
    CST_DOUBLE   of string |
    CST_NAMETYPE of cpindex * cpindex |
    CST_UTF8     of utf |
    CST_MHAND    of refkind * cpindex |
    CST_MTYPE    of cpindex |
    CST_DYN      of bmindex * cpindex |
    CST_IDYN     of bmindex * cpindex |
    CST_MOD      of cpindex |
    CST_PACK     of cpindex

  and utf =
    UTF_NAME of name |
    UTF_DESC of descriptor |
    UTF_STRI of string

  withtype cpindex = int
  and bmindex = int

  (*val top = ref 1*)
  (*fun getTop () = (top := !top + 1; !top)*)

  (*fun aux (C_CLASS cn) top = [*)
    (*CST_UTF8 (UTF_NAME (NBIN cn)),*)
    (*CST_CLASS (top + 1)]*)

    (*| aux (C_FREF   (cn, mn, dc)) top =*)
    (*(aux (C_CLASS cn) top) @ [*)
    (*CST_UTF8 (UTF_NAME (NUNQ mn)),*)
    (*CST_UTF8 (UTF_DESC dc),*)
    (*CST_FREF (top + 3, top + 4) ]*)

    (*| aux (C_MREF   (cn, mn, dc)) top =*)
    (*(aux (C_CLASS cn) top) @ [*)
    (*CST_UTF8 (UTF_NAME (NUNQ mn)),*)
    (*CST_UTF8 (UTF_DESC dc),*)
    (*CST_FREF (top + 3, top + 4)]*)

    (*| aux (C_INT i) top = [CST_INT i]*)


  (*fun flat [] top = []*)

    (*| flat (c :: cp) top = let*)
    (*val cs = aux c top in*)
    (*cs @ flat cp (top + length cs) end*)

  val i2ws2 = WordList.fromInt 2
  val i2ws4 = WordList.fromInt 4
  val i2ws8 = WordList.fromInt 8

  fun toWords (CST_CLASS    (cpindex))            = [Word.fromInt 7]
    @ (i2ws2 cpindex)
    | toWords (CST_FREF     (cpindex1, cpindex2)) = [Word.fromInt 9]
    @ (i2ws2 cpindex1) @ (i2ws2 cpindex2)
    | toWords (CST_MREF     (cpindex1, cpindex2)) = [Word.fromInt 10]
    @ (i2ws2 cpindex1) @ (i2ws2 cpindex2)
    | toWords (CST_IREF     (cpindex1, cpindex2)) = [Word.fromInt 11]
    @ (i2ws2 cpindex1) @ (i2ws2 cpindex2)
    | toWords (CST_STR      (cpindex))            = [Word.fromInt 8]
    @ (i2ws2 cpindex)
    (* TODO: ... *)
    | toWords (CST_INT      (i))                  = [Word.fromInt 3]
    @ (i2ws4 i)
    | toWords (CST_FLOAT    (r))                  = [Word.fromInt 4]
    @ [0wx0, 0wx0, 0wx0, 0wx0]
    | toWords (CST_LONG     (i))                  = [Word.fromInt 5]
    @ (i2ws8 i)
    | toWords (CST_DOUBLE   (r))                  = [Word.fromInt 6]
    @ [0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0]
    | toWords (CST_NAMETYPE (cpindex1, cpindex2)) = [Word.fromInt 12]
    @ (i2ws2 cpindex1) @ (i2ws2 cpindex2)
    | toWords (CST_UTF8     (utf))                = [Word.fromInt 1]
    @ let val bs = (case utf of
      (UTF_NAME name) => Name.toWords name
    | (UTF_DESC desc) => Descriptor.toWords desc
    | (UTF_STRI str)  => ModifiedUtf8.fromString str
      ) in
    (i2ws2 (List.length bs)) @ bs end
    | toWords (CST_MHAND    (refkind, cpindex))   = [Word.fromInt 15]
    @ [ReferenceKind.toWord refkind] @ (i2ws2 cpindex)
    | toWords (CST_MTYPE    (cpindex))            = [Word.fromInt 16]
    @ (i2ws2 cpindex)
    | toWords (CST_DYN      (bmindex, cpindex))   = [Word.fromInt 17]
    @ (i2ws2 bmindex) @ (i2ws2 cpindex)
    | toWords (CST_IDYN     (bmindex, cpindex))   = [Word.fromInt 18]
    @ (i2ws2 bmindex) @ (i2ws2 cpindex)
    | toWords (CST_MOD      (cpindex))            = [Word.fromInt 19]
    @ (i2ws2 cpindex)
    | toWords (CST_PACK     (cpindex))            = [Word.fromInt 20]
    @ (i2ws2 cpindex)

end
