structure CodeGeneration = struct

  structure IMAP = IntBinaryMapAux
  structure II = InterInstruction
  structure IP = InterProgram
  structure IC = InterClosure
  structure ICT = IntCounter
  structure LMAP = Allocation.LocationBinaryMap
  structure JD = Descriptor
  structure JI = Instruction
  structure JC = Class
  structure JM = Method
  structure JA = Attribute
  structure JF = Field

  datatype code = datatype II.code
  datatype instruction = datatype JI.instruction
  datatype constant = datatype ConstantPool.constant
  datatype classacc = datatype JC.classacc
  datatype location = datatype Allocation.location
  datatype descriptor = datatype JD.descriptor
  datatype typ   = datatype JD.typ
  datatype scon = datatype SpecialConstant.scon
  datatype clos = datatype InterClosure.closure
  datatype attributei = datatype JA.attributei

  fun id2cname i = ["C" ^ (Int.toString i)]
  fun id2fname i = "f" ^ (Int.toString i)

  val objectCname = ["java", "lang", "Object"]
  val funIname = ["Fun"]
  val tagCname = ["Tag"]
  val rcdCname = ["java", "util", "HashMap"]
  val basCname = ["Bas"]
  val packCname = ["Pack"]
  val intCname = ["java", "lang", "Integer"]

  val objTyp = JD.classTyp objectCname
  val objectDesc = JD.classDesc objectCname
  val tagDesc    = JD.classDesc tagCname

  val initDesc = M_DESC ([], V)

  fun funInitDesc id = M_DESC ([JD.classTyp (id2cname id)], V)
  val tagInitDesc = M_DESC ([I, JD.classTyp objectCname], V)

  val packInitDesc = M_DESC ([JD.classTyp tagCname], V)

  val rcdGetDesc = M_DESC ([objTyp], objTyp)
  val rcdPutDesc = M_DESC ([objTyp, objTyp], objTyp)

  val funApplyDesc  = M_DESC ([JD.classTyp objectCname], JD.classTyp objectCname)

  val intValofDesc = M_DESC ([I], JD.classTyp intCname)
  val intValDesc   = M_DESC ([], I)
  val prevDesc = JD.classDesc o id2cname

  fun genProg prog = let

  val locmap = Allocation.genProg prog
  fun closPath x y = IP.path prog x y

  fun genClos (closid, clos) = let

  fun path2clos x = closPath closid x
  val thisCname = id2cname closid

  val class = (case clos of
        FCN _ => JC.newClass ([ACC_PUBLIC], 
          id2cname closid, objectCname, [funIname])
      | TOP _ => JC.newClass ([ACC_PUBLIC], 
          id2cname closid, objectCname, []))

  val addField = JC.addField class
  val addConst = JC.addConst class

  val () = (case clos of
                TOP _ => ()
              | FCN (previd, _, _) => 
                  addField ([JF.ACC_PUBLIC], "prev", prevDesc previd))

  val previd = IC.prevClosid clos

  val objectCid = addConst (C_CLASS objectCname)
  val objInitCid = addConst (C_MREF (objectCname, "<init>", initDesc))

  val tagClsCid = addConst (C_CLASS (tagCname))
  val tagTagCid = addConst (C_FREF (tagCname, "tag", V_DESC (I)))
  val tagValCid = addConst (C_FREF (tagCname, "val", objectDesc))
  val tagInitCid = addConst (C_MREF (tagCname, "<init>", tagInitDesc))

  val packClsCid = addConst (C_CLASS (packCname))
  val packTagCid = addConst (C_FREF (packCname, "tag", tagDesc))
  val packInitCid = addConst (C_MREF (packCname, "<init>", packInitDesc))

  val rcdClsCid = addConst (C_CLASS (rcdCname))
  val rcdInitCid = addConst (C_MREF (rcdCname, "<init>", initDesc))
  val rcdGetCid = addConst (C_MREF (rcdCname, "get", rcdGetDesc))
  val rcdPutCid = addConst (C_MREF (rcdCname, "put", rcdPutDesc))

  val intValofCid = addConst (C_MREF (intCname, "valueOf", intValofDesc))
  val intValCid   = addConst (C_MREF (intCname, "intValue", intValDesc))

  fun prevCid c p = addConst (C_FREF
                  (id2cname c, "prev", JD.classDesc (id2cname p)))
  fun funClsCid  c = addConst (C_CLASS (id2cname c))
  fun funInitCid c = addConst (C_MREF (id2cname c, "<init>", 
    funInitDesc (IP.prev prog c)))
  val funApplyCid  = addConst (C_IREF (funIname, "apply", funApplyDesc))

  fun getLoc l = Option.valOf (LMAP.find (locmap, l))
  fun fld2fref (c, l) = C_FREF (id2cname c, id2fname l, objectDesc)

  fun addFld (fld as (c, l)) = let
    val class = fld2fref fld in (
    if c = closid then
    addField ([JF.ACC_PUBLIC], id2fname l, objectDesc) else ());
    addConst class end

  (* move local variable or field to/from stack top *)
  fun l2s (LOC l)      = [ALOAD l]
    | l2s (FLD (c, l)) = let
    val p = Option.valOf (path2clos c)
    val d = (length p) - 1
    val cmove = (ALOAD 0) :: (if d > 0 then (List.tabulate (d, fn n => let
      val curr = List.nth (p, n)
      val prev = List.nth (p, n+1)
      val pcid = prevCid curr prev in
      GETFIELD (pcid) end)) else [])
    val fldCid = addFld (c, l)
    val fmove = [GETFIELD fldCid] in cmove @ fmove end

    | l2s NUL = [ACONST_N]
    | l2s (BAS i) = [GETSTATIC (addConst 
        (C_FREF (basCname, id2fname i, objectDesc)))]

  fun s2l (LOC l)      = ([], [ASTORE l])
    | s2l (FLD (c, l)) = if c = closid then let
      val fldCid = addFld (c, l) in
      ([ALOAD_0], [PUTFIELD fldCid]) end else raise Size
    | s2l NUL = ([],[POP])
    | s2l (BAS _) = raise Size

  val loc2stk = l2s o getLoc

  fun stk2loc l exp = let
    val (prev, after) = (s2l o getLoc) l in
    prev @ exp @ after end

  fun genCode meth = let
    (* carries the new offset *)
    datatype ret = 
      FAIL   of JI.instruction list |
      CODE   of JI.instruction list

    val laboff = ref IMAP.empty
    fun getlo i = IMAP.find (! laboff, i)
    fun addlo i off =
      if Option.isSome (getlo i) then
      raise Size else
      laboff := IMAP.insert (! laboff, i, off)

    fun gen [] off = []

      | gen (i :: is) off = let

      fun genInst inst = let

        fun gen (MOV    (l1, l2))       =
          CODE (stk2loc l1 (loc2stk l2))

          | gen (NEWFCN (l, f))         = let
          val fCid = funClsCid f
          val init = funInitCid f in
          CODE (stk2loc l [NEW fCid, DUP, ALOAD_0, INVOKESPECIAL init]) end

          | gen (NEWSCN (l, s))         = (case s of
              INT_SCON i => CODE 
                (stk2loc l [BIPUSH i, INVOKESTATIC intValofCid]))

          | gen (NEWRCD (l, ls))        = let
          val rcode = List.foldl (fn ((l, k), c) => let
            val lload = loc2stk l
            val kCid = addConst (C_STR k)
            val kload = [LDC kCid] in
            c @ [DUP] @ kload @ lload @ [INVOKEVIRTUAL rcdPutCid, POP] end) 
            [] ls 
          val code = [NEW rcdClsCid, DUP, INVOKESPECIAL rcdInitCid] @ rcode in
          CODE (stk2loc l code) end

          | gen (NEWTAG (l1, l2, c))    = let
          val l2load = loc2stk l2 
          val code = [NEW tagClsCid, DUP, BIPUSH c] @ 
            l2load @ [INVOKESPECIAL tagInitCid] in
          CODE (stk2loc l1 code) end

          | gen (MATRCD (l1, l2, l, b)) = let
          val loop = getlo b
          val kCid = addConst (C_STR l)
          val (code0, code3) = (s2l o getLoc) l1
          val code1 = code0 @ (loc2stk l2) @ 
            [CHECKCAST rcdClsCid, LDC kCid, INVOKEVIRTUAL rcdGetCid]
          val broff = off + (JI.listSize code1)
          val reloff = (Option.valOf loop) - broff
            handle Option => 0
          val code2 = [DUP, IFNULL reloff] @ code3 
          val code = code1 @ code2 in
          if Option.isSome loop then
            CODE code else
            FAIL code end
          

          | gen (MATTAG (l1, l2, c, b)) = let
          val loop = getlo b
          val (code0, code3) = (s2l o getLoc) l1
          val code1 = code0 @ (loc2stk l2) @ 
            [DUP, GETFIELD tagTagCid, BIPUSH c]
          val broff = off + (JI.listSize code1)
          val reloff = (Option.valOf loop) - broff
            handle Option => 0
          val code2 = [IF_ICMPNE reloff, GETFIELD tagValCid] @ code3
          val code = code1 @ code2 in
          if Option.isSome loop then
            CODE code else
            FAIL code end

          | gen (MATINT (l, i, b))      = let
          val loop = getlo b
          val code1 = (loc2stk l) @ [INVOKEVIRTUAL intValCid, BIPUSH i]
          val broff = off + (JI.listSize code1)
          val refoff = (Option.valOf loop) - broff
            handle Option => 0
          val code2 = [IF_ICMPNE refoff] 
          val code = code1 @ code2 in
          if Option.isSome loop then 
            CODE code else 
            FAIL code end

          | gen (CALL   (l1, l2, l3))   = let
          val fload = loc2stk l2
          val aload = loc2stk l3
          val code = fload @ aload @ [INVOKEINTERFACE (funApplyCid, 2)] in
          CODE (stk2loc l1 code) end

          | gen (RAISE l)   = CODE ((loc2stk l) @ 
            [DUP, INVOKESPECIAL packInitCid] @ [ATHROW])

          (*| gen (IADD   (l1, l2))       = *)
          | gen (LABEL  l)              = (addlo l off; CODE [])

          | gen (II.GOTO l)                = let
          val loop = getlo l
          val reloff = (Option.valOf loop) - off
            handle Option => 0 in
          if Option.isSome loop then
          CODE [JI.GOTO reloff] else
          FAIL [JI.GOTO reloff] end

          | gen (II.RETURN l)           = CODE ((loc2stk l) @ [ARETURN])
          | gen EXIT                    = CODE ([JI.RETURN])

        val jcode = gen inst in

      (* genInst *)
      case jcode of
        CODE code => (SOME code, off + (JI.listSize code)) 
      | FAIL code => (NONE, off + (JI.listSize code)) end in let

    (* gen *)
    val (codeop, newoff) = genInst i in
      if Option.isSome codeop then
      (Option.valOf codeop) @ (gen is newoff) else let
      val rest = gen is newoff
      val (codeop, _) = genInst i in 
      (Option.valOf codeop) @ rest end end end

  (* genCode *)
  in gen meth 0 end

  (* genClos *)
  in (case clos of
    TOP (_, meth) => let
    val code = [ALOAD_0, INVOKESPECIAL objInitCid] @ (genCode meth)
    val codeattr = JC.newAttr class (CODE code) in
    JC.addMethod class ([JM.ACC_PUBLIC], "<init>", initDesc, codeattr);
    class end
  | FCN (prev, _, meth) => let
    val prevCid = (prevCid closid prev)
    val initcode = [ALOAD_0, ALOAD_1, PUTFIELD prevCid, RETURN]
    val initcodeattr = JC.newAttr class (CODE initcode)
    val code = genCode meth
    val codeattr = JC.newAttr class (CODE code) in
    JC.addMethod class ([JM.ACC_PUBLIC], "<init>", funInitDesc prev, initcodeattr);
    JC.addMethod class ([JM.ACC_PUBLIC], "apply", funApplyDesc, codeattr);
    class end) end

  (* genProg *)
  in IMAP.foldli (fn (id, c, l) =>
          (id, genClos (id, c)) :: l) [] prog end

  fun writeProg prog dir = let
    val clses = genProg prog in
    List.map (fn (id, c) => JC.write c
        (dir ^ "/" ^ "C" ^ (Int.toString id) ^ ".class")) clses; () end

  (*fun blockClosure clos = case clos of*)
      (*TOP (_, method)    => TOP (blockMethod method)*)
    (*| FCN (prev, _, method) => FCN (prev, blockMethod method) end*)

  (*fun genProg env prog =  raise Size*)

  (*and genClos env clos =  raise Size*)

  (*and infMethod env cid meth = raise Size*)

  (*and infCode env (MOV    (l1, l2)) = let *)
    (*val p1 = valOf (E.find env l1) *)
    (*handle Option => *)

    (*val p2 = E.find env l2*)

  (*fun infCode env (NEWFCN)*)
    (*| infCode env (NEWSCN)*)
    (*| infCode env (NEWRCD)*)
    (*| infCode env (NEWCON)*)
    (*| infCode env (GETRCD)*)
    (*| infCode env (GETCON)*)
    (*| infCode env (GETINT)*)
    (*| infCode env (CALL  )*)
    (*| infCode env (LABEL )*)
    (*| infCode env (RETURN)*)
end
