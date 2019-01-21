structure Instruction = struct

  datatype atype = datatype ArrayType.atype

  exception InstructionMalform

  datatype instruction =
    NOP |
    ACONST_N |
    ICONST_M |
    ICONST_0 |
    ICONST_1 |
    ICONST_2 |
    ICONST_3 |
    ICONST_4 |
    ICONST_5 |
    LCONST_0 |
    LCONST_1 |
    FCONST_0 |
    FCONST_1 |
    FCONST_2 |
    DCONST_0 |
    DCONST_1 |
    BIPUSH of int |
    SIPUSH of int |
    LDC of cpindex |
    LDC_W of cpindex |
    LDC2_W of cpindex |

    ILOAD of lvindex |
    LLOAD of lvindex |
    FLOAD of lvindex |
    DLOAD of lvindex |
    ALOAD of lvindex |
    ILOAD_0 |
    ILOAD_1 |
    ILOAD_2 |
    ILOAD_3 |
    LLOAD_0 |
    LLOAD_1 |
    LLOAD_2 |
    LLOAD_3 |
    FLOAD_0 |
    FLOAD_1 |
    FLOAD_2 |
    FLOAD_3 |
    DLOAD_0 |
    DLOAD_1 |
    DLOAD_2 |
    DLOAD_3 |
    ALOAD_0 |
    ALOAD_1 |
    ALOAD_2 |
    ALOAD_3 |
    IALOAD |
    LALOAD |
    FALOAD |
    DALOAD |
    AALOAD |
    BALOAD |
    CALOAD |
    SALOAD |

    ISTORE of lvindex |
    LSTORE of lvindex |
    FSTORE of lvindex |
    DSTORE of lvindex |
    ASTORE of lvindex |
    ISTORE_0 |
    ISTORE_1 |
    ISTORE_2 |
    ISTORE_3 |
    LSTORE_0 |
    LSTORE_1 |
    LSTORE_2 |
    LSTORE_3 |
    FSTORE_0 |
    FSTORE_1 |
    FSTORE_2 |
    FSTORE_3 |
    DSTORE_0 |
    DSTORE_1 |
    DSTORE_2 |
    DSTORE_3 |
    ASTORE_0 |
    ASTORE_1 |
    ASTORE_2 |
    ASTORE_3 |
    IASTORE |
    LASTORE |
    FASTORE |
    DASTORE |
    AASTORE |
    BASTORE |
    CASTORE |
    SASTORE |

    POP |
    POP2 |
    DUP |
    DUP_X1 |
    DUP_X2 |
    DUP2 |
    DUP2_X1 |
    DUP2_X2 |
    SWAP |

    IADD |
    LADD |
    FADD |
    DADD |
    ISUB |
    LSUB |
    FSUB |
    DSUB |
    IMUL |
    LMUL |
    FMUL |
    DMUL |
    IDIV |
    LDIV |
    FDIV |
    DDIV |
    IREM |
    LREM |
    FREM |
    DREM |
    INEG |
    LNEG |
    FNEG |
    DNEG |
    ISHL |
    LSHL |
    ISHR |
    LSHR |
    IUSHR |
    LUSHR |
    IAND |
    LAND |
    IOR |
    LOR |
    IXOR |
    LXOR |
    IINC of lvindex * int|

    I2L |
    I2F |
    I2D |
    L2I |
    L2F |
    L2D |
    F2I |
    F2L |
    F2D |
    D2I |
    D2L |
    D2F |
    I2B |
    I2C |
    I2S |

    LCMP |
    FCMPL |
    FCMPG |
    DCMPL |
    DCMPG |
    IFEQ of offset |
    IFNE of offset |
    IFLT of offset |
    IFGE of offset |
    IFGT of offset |
    IFLE of offset |
    IF_ICMPEQ of offset |
    IF_ICMPNE of offset |
    IF_ICMPLT of offset |
    IF_ICMPGE of offset |
    IF_ICMPGT of offset |
    IF_ICMPLE of offset |
    IF_ACMPEQ of offset |
    IF_ACMPNE of offset |

    GETSTATIC of cpindex |
    PUTSTATIC of cpindex |
    GETFIELD of cpindex |
    PUTFIELD of cpindex |
    INVOKEVIRTUAL of cpindex |
    INVOKESPECIAL of cpindex |
    INVOKESTATIC of cpindex |
    INVOKEINTERFACE of cpindex * count |
    INVOKEDYNAMIC of cpindex |
    NEW of cpindex |
    NEWARRAY of atype |
    ANEWARRAY of cpindex |
    ARRAYLENGTH |
    ATHROW |
    CHECKCAST of cpindex |
    INSTANCEOF of cpindex |
    MONITORENTER |
    MONITOREXIT |

    GOTO of offset |
    JSR of offset |
    RET of lvindex |
    (* default, low, high, offset list *)
    TABLESWITCH of offset * offset * offset * offset list |
    LOOKUPSWITCH of offset * count * (match * offset) list |
    IRETURN |
    LRETURN |
    FRETURN |
    DRETURN |
    ARETURN |
    RETURN |

    WIDE of instruction |
    MULTIANEWARRAY of cpindex * int|
    IFNULL of offset |
    IFNONNULL of offset |
    GOTO_W of offset |
    JSR_W of offset |

    BREAKPOINT |
    IMPDEP1 |
    IMPDEP2

  withtype offset = int
  and lvindex = int
  and cpindex = int
  and match = int
  and count = int

  val i2ws = WordList.fromInt

  fun toWords NOP      = [0wx00]
    | toWords ACONST_N = [0wx01]
    | toWords ICONST_M = [0wx02]
    | toWords ICONST_0 = [0wx03]
    | toWords ICONST_1 = [0wx04]
    | toWords ICONST_2 = [0wx05]
    | toWords ICONST_3 = [0wx06]
    | toWords ICONST_4 = [0wx07]
    | toWords ICONST_5 = [0wx08]
    | toWords LCONST_0 = [0wx09]
    | toWords LCONST_1 = [0wx0a]
    | toWords FCONST_0 = [0wx0b]
    | toWords FCONST_1 = [0wx0c]
    | toWords FCONST_2 = [0wx0d]
    | toWords DCONST_0 = [0wx0e]
    | toWords DCONST_1 = [0wx0f]
    | toWords (BIPUSH i)   = [0wx10] @ (i2ws 1 i)
    | toWords (SIPUSH i)   = [0wx11] @ (i2ws 2 i)
    | toWords (LDC    cpindex)   = [0wx12] @ (i2ws 1 cpindex)
    | toWords (LDC_W  cpindex)   = [0wx13] @ (i2ws 2 cpindex)
    | toWords (LDC2_W cpindex)   = [0wx14] @ (i2ws 2 cpindex)

    | toWords (ILOAD lvindex)   = [0wx15] @ (i2ws 1 lvindex)
    | toWords (LLOAD lvindex)   = [0wx16] @ (i2ws 1 lvindex)
    | toWords (FLOAD lvindex)   = [0wx17] @ (i2ws 1 lvindex)
    | toWords (DLOAD lvindex)   = [0wx18] @ (i2ws 1 lvindex)
    | toWords (ALOAD lvindex)   = [0wx19] @ (i2ws 1 lvindex)
    | toWords ILOAD_0 = [0wx1a]
    | toWords ILOAD_1 = [0wx1b]
    | toWords ILOAD_2 = [0wx1c]
    | toWords ILOAD_3 = [0wx1d]
    | toWords LLOAD_0 = [0wx1e]
    | toWords LLOAD_1 = [0wx1f]
    | toWords LLOAD_2 = [0wx20]
    | toWords LLOAD_3 = [0wx21]
    | toWords FLOAD_0 = [0wx22]
    | toWords FLOAD_1 = [0wx23]
    | toWords FLOAD_2 = [0wx24]
    | toWords FLOAD_3 = [0wx25]
    | toWords DLOAD_0 = [0wx26]
    | toWords DLOAD_1 = [0wx27]
    | toWords DLOAD_2 = [0wx28]
    | toWords DLOAD_3 = [0wx29]
    | toWords ALOAD_0 = [0wx2a]
    | toWords ALOAD_1 = [0wx2b]
    | toWords ALOAD_2 = [0wx2c]
    | toWords ALOAD_3 = [0wx2d]
    | toWords IALOAD  = [0wx2e]
    | toWords LALOAD  = [0wx2f]
    | toWords FALOAD  = [0wx30]
    | toWords DALOAD  = [0wx31]
    | toWords AALOAD  = [0wx32]
    | toWords BALOAD  = [0wx33]
    | toWords CALOAD  = [0wx34]
    | toWords SALOAD  = [0wx35]

    | toWords (ISTORE lvindex)   = [0wx36] @ (i2ws 1 lvindex)
    | toWords (LSTORE lvindex)   = [0wx37] @ (i2ws 1 lvindex)
    | toWords (FSTORE lvindex)   = [0wx38] @ (i2ws 1 lvindex)
    | toWords (DSTORE lvindex)   = [0wx39] @ (i2ws 1 lvindex)
    | toWords (ASTORE lvindex)   = [0wx3a] @ (i2ws 1 lvindex)
    | toWords ISTORE_0 = [0wx3b]
    | toWords ISTORE_1 = [0wx3c]
    | toWords ISTORE_2 = [0wx3d]
    | toWords ISTORE_3 = [0wx3e]
    | toWords LSTORE_0 = [0wx3f]
    | toWords LSTORE_1 = [0wx40]
    | toWords LSTORE_2 = [0wx41]
    | toWords LSTORE_3 = [0wx42]
    | toWords FSTORE_0 = [0wx43]
    | toWords FSTORE_1 = [0wx44]
    | toWords FSTORE_2 = [0wx45]
    | toWords FSTORE_3 = [0wx46]
    | toWords DSTORE_0 = [0wx47]
    | toWords DSTORE_1 = [0wx48]
    | toWords DSTORE_2 = [0wx49]
    | toWords DSTORE_3 = [0wx4a]
    | toWords ASTORE_0 = [0wx4b]
    | toWords ASTORE_1 = [0wx4c]
    | toWords ASTORE_2 = [0wx4d]
    | toWords ASTORE_3 = [0wx4e]
    | toWords IASTORE  = [0wx4f]
    | toWords LASTORE  = [0wx50]
    | toWords FASTORE  = [0wx51]
    | toWords DASTORE  = [0wx52]
    | toWords AASTORE  = [0wx53]
    | toWords BASTORE  = [0wx54]
    | toWords CASTORE  = [0wx55]
    | toWords SASTORE  = [0wx56]

    | toWords POP     = [0wx57]
    | toWords POP2    = [0wx58]
    | toWords DUP     = [0wx59]
    | toWords DUP_X1  = [0wx5a]
    | toWords DUP_X2  = [0wx5b]
    | toWords DUP2    = [0wx5c]
    | toWords DUP2_X1 = [0wx5d]
    | toWords DUP2_X2 = [0wx5e]
    | toWords SWAP    = [0wx5f]

    | toWords IADD  = [0wx60]
    | toWords LADD  = [0wx61]
    | toWords FADD  = [0wx62]
    | toWords DADD  = [0wx63]
    | toWords ISUB  = [0wx64]
    | toWords LSUB  = [0wx65]
    | toWords FSUB  = [0wx66]
    | toWords DSUB  = [0wx67]
    | toWords IMUL  = [0wx68]
    | toWords LMUL  = [0wx69]
    | toWords FMUL  = [0wx6a]
    | toWords DMUL  = [0wx6b]
    | toWords IDIV  = [0wx6c]
    | toWords LDIV  = [0wx6d]
    | toWords FDIV  = [0wx6e]
    | toWords DDIV  = [0wx6f]
    | toWords IREM  = [0wx70]
    | toWords LREM  = [0wx71]
    | toWords FREM  = [0wx72]
    | toWords DREM  = [0wx73]
    | toWords INEG  = [0wx74]
    | toWords LNEG  = [0wx75]
    | toWords FNEG  = [0wx76]
    | toWords DNEG  = [0wx77]
    | toWords ISHL  = [0wx78]
    | toWords LSHL  = [0wx79]
    | toWords ISHR  = [0wx7a]
    | toWords LSHR  = [0wx7b]
    | toWords IUSHR = [0wx7c]
    | toWords LUSHR = [0wx7d]
    | toWords IAND  = [0wx7e]
    | toWords LAND  = [0wx7f]
    | toWords IOR   = [0wx80]
    | toWords LOR   = [0wx81]
    | toWords IXOR  = [0wx82]
    | toWords LXOR  = [0wx83]
    | toWords (IINC (lvindex, inc))  = [0wx84] @ (i2ws 1 lvindex) @ (i2ws 1 inc)

    | toWords I2L = [0wx85]
    | toWords I2F = [0wx86]
    | toWords I2D = [0wx87]
    | toWords L2I = [0wx88]
    | toWords L2F = [0wx89]
    | toWords L2D = [0wx8a]
    | toWords F2I = [0wx8b]
    | toWords F2L = [0wx8c]
    | toWords F2D = [0wx8d]
    | toWords D2I = [0wx8e]
    | toWords D2L = [0wx8f]
    | toWords D2F = [0wx90]
    | toWords I2B = [0wx91]
    | toWords I2C = [0wx92]
    | toWords I2S = [0wx93]

    | toWords LCMP      = [0wx94]
    | toWords FCMPL     = [0wx95]
    | toWords FCMPG     = [0wx96]
    | toWords DCMPL     = [0wx97]
    | toWords DCMPG     = [0wx98]
    | toWords (IFEQ      offset) = [0wx99] @ (i2ws 2 offset)
    | toWords (IFNE      offset) = [0wx9a] @ (i2ws 2 offset)
    | toWords (IFLT      offset) = [0wx9b] @ (i2ws 2 offset)
    | toWords (IFGE      offset) = [0wx9c] @ (i2ws 2 offset)
    | toWords (IFGT      offset) = [0wx9d] @ (i2ws 2 offset)
    | toWords (IFLE      offset) = [0wx9e] @ (i2ws 2 offset)
    | toWords (IF_ICMPEQ offset) = [0wx9f] @ (i2ws 2 offset)
    | toWords (IF_ICMPNE offset) = [0wxa0] @ (i2ws 2 offset)
    | toWords (IF_ICMPLT offset) = [0wxa1] @ (i2ws 2 offset)
    | toWords (IF_ICMPGE offset) = [0wxa2] @ (i2ws 2 offset)
    | toWords (IF_ICMPGT offset) = [0wxa3] @ (i2ws 2 offset)
    | toWords (IF_ICMPLE offset) = [0wxa4] @ (i2ws 2 offset)
    | toWords (IF_ACMPEQ offset) = [0wxa5] @ (i2ws 2 offset)
    | toWords (IF_ACMPNE offset) = [0wxa6] @ (i2ws 2 offset)

    | toWords (GETSTATIC     cpindex)   = [0wxb2] @ (i2ws 2 cpindex)
    | toWords (PUTSTATIC     cpindex)   = [0wxb3] @ (i2ws 2 cpindex)
    | toWords (GETFIELD      cpindex)   = [0wxb4] @ (i2ws 2 cpindex)
    | toWords (PUTFIELD      cpindex)   = [0wxb5] @ (i2ws 2 cpindex)
    | toWords (INVOKEVIRTUAL cpindex)   = [0wxb6] @ (i2ws 2 cpindex)
    | toWords (INVOKESPECIAL cpindex)   = [0wxb7] @ (i2ws 2 cpindex)
    | toWords (INVOKESTATIC  cpindex)   = [0wxb8] @ (i2ws 2 cpindex)
    | toWords (INVOKEINTERFACE (cpindex, count))
    = [0wxb9] @ (i2ws 2 cpindex) @ (i2ws 1 count) @ [0wx0]
    | toWords (INVOKEDYNAMIC cpindex)   = [0wxba] @ (i2ws 2 cpindex) @ [0wx0, 0wx0]
    | toWords (NEW           cpindex)   = [0wxbb] @ (i2ws 2 cpindex)
    | toWords (NEWARRAY      arrty)   = [0wxbc] @ [ArrayType.toWord arrty]
    | toWords (ANEWARRAY     cpindex)   = [0wxbd] @ (i2ws 2 cpindex)
    | toWords ARRAYLENGTH     = [0wxbe]
    | toWords ATHROW          = [0wxbf]
    | toWords (CHECKCAST  cpindex)      = [0wxc0] @ (i2ws 2 cpindex)
    | toWords (INSTANCEOF cpindex)      = [0wxc1] @ (i2ws 2 cpindex)
    | toWords MONITORENTER    = [0wxc2]
    | toWords MONITOREXIT     = [0wxc3]

    | toWords (GOTO offset)         = [0wxa7] @ (i2ws 2 offset)
    | toWords (JSR  offset)         = [0wxa8] @ (i2ws 2 offset)
    | toWords (RET lvindex)          = [0wxa9] @ (i2ws 1 lvindex)
    | toWords (TABLESWITCH (default, low, high, offsetList)) =
    [0wxaa] @ (i2ws 4 default) @ (i2ws 4 low) @ (i2ws 4 default) @
    (List.concat (List.map (i2ws 4) offsetList))
    | toWords (LOOKUPSWITCH (default, count, keyOffsetList)) =
    [0wxab] @ (i2ws 4 default) @ (i2ws 4 count) @
    (List.concat (List.map (fn (key, off) => (i2ws 4 key) @ (i2ws 4 off))
    keyOffsetList))
    | toWords IRETURN      = [0wxac]
    | toWords LRETURN      = [0wxad]
    | toWords FRETURN      = [0wxae]
    | toWords DRETURN      = [0wxaf]
    | toWords ARETURN      = [0wxb0]
    | toWords RETURN       = [0wxb1]

    | toWords (WIDE ins) = [0wxc4] @ (case ins of
      (IINC (lvindex, inc)) =>
      [0wx84] @ (i2ws 2 lvindex) @ (i2ws 2 inc)
    | (ILOAD lvindex)   => [0wx15] @ (i2ws 2 lvindex)
    | (LLOAD lvindex)   => [0wx16] @ (i2ws 2 lvindex)
    | (FLOAD lvindex)   => [0wx17] @ (i2ws 2 lvindex)
    | (DLOAD lvindex)   => [0wx18] @ (i2ws 2 lvindex)
    | (ALOAD lvindex)   => [0wx19] @ (i2ws 2 lvindex)
    | (ISTORE lvindex)  => [0wx36] @ (i2ws 2 lvindex)
    | (LSTORE lvindex)  => [0wx37] @ (i2ws 2 lvindex)
    | (FSTORE lvindex)  => [0wx38] @ (i2ws 2 lvindex)
    | (DSTORE lvindex)  => [0wx39] @ (i2ws 2 lvindex)
    | (ASTORE lvindex)  => [0wx3a] @ (i2ws 2 lvindex)
    | (RET lvindex)     => [0wxa9] @ (i2ws 2 lvindex)
    | _ => raise InstructionMalform)
    | toWords (MULTIANEWARRAY (cpindex, dim)) =
    [0wxc5] @ (i2ws 2 cpindex) @ (i2ws 1 dim)
    | toWords (IFNULL    offset)      = [0wxc6] @ (i2ws 2 offset)
    | toWords (IFNONNULL offset)      = [0wxc7] @ (i2ws 2 offset)
    | toWords (GOTO_W    offset)      = [0wxc8] @ (i2ws 4 offset)
    | toWords (JSR_W     offset)      = [0wxc9] @ (i2ws 4 offset)

    | toWords BREAKPOINT = [0wxca]
    | toWords IMPDEP1    = [0wxfe]
    | toWords IMPDEP2    = [0wxff]

end
