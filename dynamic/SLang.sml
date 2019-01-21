structure Slang = struct 

  structure CST = Constant
  structure CP = ConstantPool
  structure JI = JavaItem
  structure F = Field
  structure M = Method
  structure I = Instruction

  datatype utf = datatype Constant.utf
  datatype descriptor = datatype Descriptor.descriptor
  datatype name = datatype Name.name
  datatype instruction = datatype Instruction.instruction
  datatype constant = datatype CP.constant
  datatype attribute = datatype Attribute.attribute

  datatype instruction = 
    CONST of const |

    (*LOAD of int |*)
    (*STORE of int |*)

    GET of int |
    PUT of int |
    GETF of int |

    STORE of int |
    LOAD of int |

    NRCD of int |
    NCON of int |
    DRCD of int |
    DCON of int |

    CALL of int |
    RETURN |

    IADD |
    ISUB |
    IMUL |
    IDIV |
    INEG |
    RADD |
    RSUB |
    RMUL |
    RDIV |
    RNEG

  and const =
    I of int |
    R of real

  type method = (string * instruction list)

  val cidObj = 4

  val name = ["Top"]

  fun genClass (ms, init, spa) = let

    val class = JavaItem.newClass ["Top"]
    val cpref = #1 class
    val fpref = #6 class 
    val mpref = #7 class
    val apref = #8 class

    val cidEnv     = CP.radd cpref (C_FREF (name, "env", JI.envDesc))
    val cidFenv    = CP.radd cpref (C_FREF (name, "fenv", JI.fenvDesc))
    val cidCodetag = CP.radd cpref (C_UTF (UTF_STRI "Code"))
    val cidMhClass = CP.radd cpref (C_CLASS JI.methodHandleName)
    val cidIvalof  = CP.radd cpref (C_MREF (JI.iCname, "valueOf",
    JI.ivalofDesc))

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

    fun genInst (CONST c) = (case c of I i => 
      [SIPUSH i, INVOKESTATIC cidIvalof])
      | genInst (PUT i) = [GETSTATIC cidEnv, SWAP, SIPUSH i, SWAP, AASTORE]
      | genInst (GET i) = [GETSTATIC cidEnv, SIPUSH i, AALOAD]
      | genInst (GETF i) = [GETSTATIC cidFenv, SIPUSH i, AALOAD]
      | genInst (STORE i) = [ASTORE i]
      | genInst (LOAD i) = [ALOAD i]
      | genInst (RETURN) = [ARETURN]

    fun genInsts is = (List.concat o (List.map genInst)) is

    fun genCode cb = ATTR_CODE (cidCodetag, cb, [], [])

    val clinitInsts = preclinitInsts @ (genInsts init) @ [I.RETURN]
    val clinitCodeAttr = genCode clinitInsts

    fun genMethod desc (name, is) = let
      val nameId = CP.radd cpref (C_UTF (UTF_NAME (NUNQ name)))
      val descId = CP.radd cpref (C_UTF (UTF_DESC desc))       
      val codeAttr = genCode (genInsts is) in
      ([Method.ACC_STATIC], nameId, descId, [codeAttr]) end

    val () = Method.radd mpref 
      ("<clinit>", JI.initDesc, clinitCodeAttr) cpref

    val msc   = map (genMethod JI.methodDesc) ms in

    mpref := (! mpref) @ msc;
    class end

end
