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

  datatype instruction =
    CONST of const |
    CONSTI of int |

    (*LOAD of int |*)
    (*STORE of int |*)

    GET of int |
    PUT of int |
    GETF of int |

    STORE of int |
    LOAD of int |

    NEWRCD |
    PUTRCD |

    NEWCON |
    PUTCON |

    GETRCD of string |
    GETCON of int |

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

  type method = (string * instruction list)

  val cidObj = 4

  val topCname = ["Top"]

  fun genClass (ms, init, spa) = let

    val class = JavaItem.newClass ["Top"]
    val cpref = #1 class
    val fpref = #6 class
    val mpref = #7 class
    val apref = #8 class

    val cidEnv     = CP.radd cpref (C_FREF (topCname, "env", JI.envDesc))
    val cidFenv    = CP.radd cpref (C_FREF (topCname, "fenv", JI.fenvDesc))
    val cidCodetag = CP.radd cpref (C_UTF (UTF_STRI "Code"))
    val cidMhClass = CP.radd cpref (C_CLASS JI.methodHandleName)
    val cidIvalof  = CP.radd cpref (C_MREF (JI.iCname, "valueOf",
    JI.ivalofDesc))
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

    val cidConGet = CP.radd cpref (C_MREF (JI.conCname, "get",
    JI.conGetDesc))
    val cidRcdGet = CP.radd cpref (C_MREF (JI.mapCname, "get",
    JI.methodDesc))

    val preclinitInsts = [
      SIPUSH 10000,
      ANEWARRAY cidObj,
      PUTSTATIC cidEnv,
      SIPUSH 10000,
      ANEWARRAY cidMhClass,
      PUTSTATIC cidFenv
    ]

    val () = F.radd fpref ("env", JI.envDesc) cpref
    val () = F.radd fpref ("fenv", JI.fenvDesc) cpref

    fun genInst (CONST c) = (case c of 
        I i =>
      [SIPUSH i, INVOKESTATIC cidIvalof]
      | S s => [LDC (CP.radd cpref (C_STR s))])
      | genInst (PUT i) = [GETSTATIC cidEnv, SWAP, SIPUSH i, SWAP, AASTORE]
      | genInst (GET i) = [GETSTATIC cidEnv, SIPUSH i, AALOAD]
      | genInst (GETF i) = [GETSTATIC cidFenv, SIPUSH i, AALOAD]
      | genInst (STORE i) = [ASTORE i]
      | genInst (LOAD i) = [ALOAD i]
      | genInst (CALL) = [SWAP, CHECKCAST cidMhClass, SWAP, INVOKEVIRTUAL
      cidInvoke]
      | genInst (NEWRCD) =
      [NEW cidMapClass, DUP, INVOKESPECIAL cidMapInit]
      | genInst (PUTRCD) = [INVOKEVIRTUAL cidMapPut]
      | genInst (GETRCD s) = 
      [CHECKCAST cidMapClass, LDC (CP.radd cpref (C_STR s)), INVOKEVIRTUAL
      cidRcdGet]
      | genInst NEWCON = [NEW cidConClass]
      | genInst PUTCON = [INVOKESPECIAL cidConPut]
      | genInst (GETCON i) = [CHECKCAST cidConClass, SIPUSH i, INVOKESTATIC
      cidIvalof, INVOKEVIRTUAL cidConGet]
      | genInst DUPL = [DUP]
      | genInst REMO = [POP]
      | genInst RETURN = [ARETURN]
      | genInst (MATCH i) = [POP]

    fun genInsts is = (List.concat o (List.map genInst)) is

    fun genAttrCode cb = ATTR_CODE (cidCodetag, cb, [], [])

    val fclinitInsts = List.foldl (fn ((name, insts), clinit) => ( let
      val m = (name, JI.methodDesc, (genAttrCode o genInsts) insts)
      val cidMh = CP.radd cpref (C_MHANDLE (REF_INVST,
        (topCname, name, JI.methodDesc)))
      val count = (length clinit) div 4 in
      M.radd mpref m cpref;
      clinit @ [GETSTATIC cidFenv, SIPUSH count, LDC cidMh, AASTORE]
      end)) [] ms

    val clinitInsts = preclinitInsts @ fclinitInsts @ 
      (genInsts init) @ [I.RETURN]

    val clinitCodeAttr = genAttrCode clinitInsts

    val () = Method.radd mpref
      ("<clinit>", JI.initDesc, clinitCodeAttr) cpref

        in class end

end
