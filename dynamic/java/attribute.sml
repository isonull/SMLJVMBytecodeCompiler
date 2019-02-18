structure Attribute = struct

  structure CP = ConstantPool

  datatype instruction = datatype Instruction.instruction
  datatype constant = datatype CP.constant
  datatype utf = datatype CP.utf

  datatype attribute =
    ATTR_CONST of tagindex * cpindex |
    ATTR_CODE  of tagindex * instruction list * except list * attribute list |
    ATTR_BOOTS of tagindex * bootstrap list |
    ATTR_NHOST of tagindex * cpindex |
    ATTR_NMEMB of tagindex * cpindex list

  withtype pc = int
  and cpindex = int
  and except = pc * pc * pc * cpindex
  and bootstrap = cpindex * (cpindex list)
  and tagindex = cpindex

  val i2ws2 = WordList.fromInt 2
  val i2ws4 = WordList.fromInt 4

  val MAXSTACK = 100
  val MAXLOCAL = 100

  datatype attributei = CODE of instruction list

  fun new cpref (CODE insts) = let
    val tag = CP.radd cpref (C_UTF (UTF_STRI "Code")) in
    ATTR_CODE (tag, insts, [], []) end

  fun toWords (ATTR_CONST (tagindex, cpindex)) =
    (i2ws2 tagindex) @ (i2ws2 cpindex)

    | toWords (ATTR_CODE  (tagindex, inss, excs, atts)) = let
    val insWords = (List.concat o (List.map Instruction.toWords)) inss
    val insLen = i2ws4 (List.length insWords) 
    val body = (i2ws2 MAXSTACK) @ (i2ws2 MAXLOCAL) 
    @ insLen @ insWords @ (i2ws2 0)@ (i2ws2 0) in
    (i2ws2 tagindex) @ (i2ws4 (length body)) @ body end

    | toWords (ATTR_BOOTS (tagindex, bms)) =  let
    fun bootstrapAux (cpindex, cpindexs) = let
    val bsref = i2ws2 cpindex
    val numbs = i2ws2 (List.length cpindexs)
    val args  = (List.concat o (List.map i2ws2)) cpindexs in 
    bsref @ numbs @ args end in
    (i2ws2 tagindex) @ (i2ws2 (List.length bms)) @ 
    (List.concat o (List.map bootstrapAux)) bms end

    | toWords (ATTR_NHOST (tagindex, cpindex))          = raise Size
    | toWords (ATTR_NMEMB (tagindex, cpindexs))         = raise Size


end
