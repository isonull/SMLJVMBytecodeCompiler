structure Slang = struct

  structure CST = Constant
  structure CP = ConstantPool
  structure JI = JavaItem
  structure F = Field
  structure M = Method
  structure I = Instruction
  structure RK = ReferenceKind

  datatype utf = datatype Constant.utf
  datatype descriptor = datatype Descriptor.descriptor
  datatype name = datatype Name.name
  datatype instruction = datatype Instruction.instruction
  datatype constant = datatype CP.constant
  datatype attribute = datatype Attribute.attribute
  datatype refkind = datatype RK.refkind
  datatype value = datatype Value.value

  datatype instruction =
    CONST of const |
    CONSTI of int |

    (*LOAD of int |*)
    (*STORE of int |*)

    GET of value |
    PUT of value |
    GETF of int |

    STORE of int |
    LOAD of int |

    NEWRCD |
    PUTRCD |

    NEWCON |
    PUTCON of bool |

    GETRCD of string * int |
    GETCON of int * bool * int |

    CALL |
    RETURN |

    DUPL |
    REMO |

    MATCH of int |

    IADD |
    ISUB |
    IMUL |
    IDIV |
    INEG 

  and const =
    I of int |
    R of real |
    S of string

  type block  = instruction list
  type method = (string * (block list))

  val cidObj = 4

  val topCname = ["Top"]

  fun genClass (ms, init, spa) = let

    val class = JavaItem.newClass ["Top"]
    val cpref = #1 class
    val fpref = #6 class
    val mpref = #7 class
    val apref = #8 class

    val cidEnvFref     = CP.radd cpref (C_FREF (topCname, "env", JI.envDesc))
    val cidFenvFref    = CP.radd cpref (C_FREF (topCname, "fenv", JI.fenvDesc))
    val cidCodetag = CP.radd cpref (C_UTF (UTF_STRI "Code"))
    val cidMhClass = CP.radd cpref (C_CLASS JI.methodHandleName)
    val cidIvalof  = CP.radd cpref (C_MREF (JI.iCname, "valueOf",
    JI.ivalofDesc))
    val cidIClass  = CP.radd cpref (C_CLASS JI.iCname)
    val cidIval    = CP.radd cpref (C_MREF (JI.iCname, "intValue",
    JI.ivalDesc))
    val cidInvoke  = CP.radd cpref (C_MREF (JI.methodHandleName, "invoke",
    JI.methodDesc))
    val cidMapClass= CP.radd cpref (C_CLASS (JI.mapCname))
    val cidMapInit = CP.radd cpref (C_MREF (JI.mapCname, "<init>",
    JI.initDesc))
    val cidMapPut = CP.radd cpref (C_MREF (JI.mapCname, "put",
    JI.putDesc))

    val cidConClass = CP.radd cpref (C_CLASS (JI.conCname))
    val cidConPut = CP.radd cpref (C_MREF (JI.conCname, "<init>",
    JI.conPutDesc))
    val cidConPut0 = CP.radd cpref (C_MREF (JI.conCname, "<init>",
    JI.conPut0Desc))
    val cidTagField = CP.radd cpref (C_FREF (JI.conCname, "tag", JI.tagDesc))
    val cidValField = CP.radd cpref (C_FREF (JI.conCname, "val",
    JI.objDesc))
    val cidConGet = CP.radd cpref (C_MREF (JI.conCname, "get",
    JI.conGetDesc))
    val cidRcdGet = CP.radd cpref (C_MREF (JI.mapCname, "get",
    JI.methodDesc))

    val preclinitInsts = [
      SIPUSH 10000,
      ANEWARRAY cidObj,
      PUTSTATIC cidEnvFref,
      SIPUSH 10000,
      ANEWARRAY cidMhClass,
      PUTSTATIC cidFenvFref
    ]

    val () = F.radd fpref ("env", JI.envDesc) cpref
    val () = F.radd fpref ("fenv", JI.fenvDesc) cpref

    fun genBlock is = let
      val relBr = ref 0
    
      fun genInst (CONST c) = (case c of 
          I i =>
        [SIPUSH i, INVOKESTATIC cidIvalof]
        | S s => [LDC (CP.radd cpref (C_STR s))])

        | genInst (PUT loc) = (case loc of
          GLB i => [GETSTATIC cidEnvFref, SWAP, SIPUSH i, SWAP, AASTORE]
        | LOC i => [ASTORE i])

        | genInst (GET loc) = (case loc of 
          GLB i => [GETSTATIC cidEnvFref, SIPUSH i, AALOAD]
        | LOC i => [ALOAD i])
        | genInst (GETF i) = [GETSTATIC cidFenvFref, SIPUSH i, AALOAD]
        | genInst (STORE i) = [ASTORE i]
        | genInst (LOAD i) = [ALOAD i]
        | genInst (CALL) = [SWAP, CHECKCAST cidMhClass, SWAP, INVOKEVIRTUAL
        cidInvoke]
        | genInst (NEWRCD) =
        [NEW cidMapClass, DUP, INVOKESPECIAL cidMapInit]
        | genInst (PUTRCD) = [INVOKEVIRTUAL cidMapPut]
        | genInst (GETRCD (s, l)) =
        [CHECKCAST cidMapClass, LDC (CP.radd cpref (C_STR s)), INVOKEVIRTUAL
        cidRcdGet, DUP, ASTORE l, IFNULL ((! relBr) + 5), ALOAD l]
        | genInst NEWCON = [NEW cidConClass]
        | genInst (PUTCON hasval) = if hasval then
          [INVOKESPECIAL cidConPut] else
          [INVOKESPECIAL cidConPut0]
        | genInst (GETCON (i, hasval, l)) = [
        CHECKCAST cidConClass, DUP, ASTORE l,
        GETFIELD cidTagField, INVOKEVIRTUAL cidIval, SIPUSH i] @
        (if hasval then 
          [IF_ICMPNE ((! relBr) + 8), ALOAD l, GETFIELD cidValField]
         else
          [IF_ICMPNE ((! relBr) + 3)])

        | genInst DUPL = [DUP]
        | genInst REMO = [POP]
        | genInst RETURN = [ARETURN]
        | genInst (MATCH i) = 
        [CHECKCAST cidIClass,INVOKEVIRTUAL cidIval, SIPUSH i,
         IF_ICMPNE ((! relBr) + 3)] in

      List.foldr (fn (is, out) => let
        val inst = genInst is 
        val size = List.foldl (fn (i, s) => (I.size i) + s) 0 inst in
        relBr := (! relBr) + size;
        inst @ out end
      ) [] is end

    fun genMethod is = (List.concat o (List.map genBlock)) is

    fun genAttrCode cb = ATTR_CODE (cidCodetag, cb, [], [])

    val fclinitInsts = List.foldl (fn ((name, insts), clinit) => ( let
      val m = (name, JI.methodDesc, (genAttrCode o genMethod) insts)
      val cidMh = CP.radd cpref (C_MHANDLE (REF_INVST,
        (topCname, name, JI.methodDesc)))
      val count = (length clinit) div 4 in
      M.radd mpref m cpref;
      clinit @ [GETSTATIC cidFenvFref, SIPUSH count, LDC cidMh, AASTORE]
      end)) [] ms

    val clinitInsts = preclinitInsts @ fclinitInsts @ 
      (genBlock init) @ [I.RETURN]

    val clinitCodeAttr = genAttrCode clinitInsts

    val () = Method.radd mpref
      ("<clinit>", JI.initDesc, clinitCodeAttr) cpref

        in class end

end
