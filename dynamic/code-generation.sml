structure NewCodeGeneration = struct

  structure IMAP = IntBinaryMapAux
  structure II = InterInstruction
  structure IP = InterProgram
  structure IC = InterClosure
  structure ICT = IntCounter
  structure LMAP = LocationBinaryMap
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
  datatype location = datatype NewAllocation.location
  datatype descriptor = datatype JD.descriptor
  datatype typ   = datatype JD.typ
  datatype scon = datatype SpecialConstant.scon
  datatype clos = datatype InterClosure.closure
  datatype attributei = datatype JA.attributei

  val intListSort = ListMergeSort.sort (op >)

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

  val tagInitDesc = M_DESC ([I, JD.classTyp objectCname], V)

  val packInitDesc = M_DESC ([JD.classTyp tagCname], V)

  val rcdGetDesc = M_DESC ([objTyp], objTyp)
  val rcdPutDesc = M_DESC ([objTyp, objTyp], objTyp)

  val funApplyDesc  = M_DESC ([JD.classTyp objectCname], JD.classTyp objectCname)

  val intValofDesc = M_DESC ([I], JD.classTyp intCname)
  val intValDesc   = M_DESC ([], I)
  val prevDesc = JD.classDesc o id2cname

  fun genProg prog = let

  val glocmap = NewAllocation.genProg prog
  val gfldmap = IMAP.map (fn lmap =>
    intListSort (LMAP.foldl (fn (FLD f, fs) => f :: fs
                              | (_, fs) => fs) [] lmap)) glocmap

  val gfrevmap = IMAP.map (fn lmap =>
    (LMAP.foldli (fn (loc, FLD f, fm) => IMAP.insert (fm, f, loc)
                   | (loc, _,     fm) => fm) IMAP.empty lmap)) glocmap

  fun funInitDesc id = let val narg = (length o valOf) (IMAP.find (gfldmap, id)) in
    M_DESC ((List.tabulate (narg, (fn _ => objTyp))), V) end
    handle Option => (print "funInitDesc"; raise Option)

  (* construct a closure t from closure s *)
  fun getFlds c = valOf (IMAP.find (gfldmap, c)) 
    handle Option => (print "GETFLD"; raise Option)

  (*val locmap = Allocation.genProg prog*)
  fun closPath x y = IP.path prog x y

  fun genClos (closid, clos) = let

  (* target fid -> target cid -> current cid -> current loc *)
  (*  target clos , target fldid *)
  (* get external field location *)
  fun getExfloc t f = let
    val frevmap = valOf (IMAP.find (gfrevmap, t))
    val loc = valOf (IMAP.find (frevmap, f)) in loc end
    handle Option => (print "GetExfloc"; raise Option)


  val locmap = (Option.valOf (IMAP.find (glocmap, closid))) : (location
  LMAP.map) handle Option => (print "locmap"; raise Option)

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
              | FCN (previd, _, _, _) =>
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

  val arrayCid = addConst (C_MREF (rcdCname, "put", rcdPutDesc))

  val intValofCid = addConst (C_MREF (intCname, "valueOf", intValofDesc))
  val intValCid   = addConst (C_MREF (intCname, "intValue", intValDesc))

  fun prevCid c p = addConst (C_FREF
                  (id2cname c, "prev", JD.classDesc (id2cname p)))
  fun funClsCid  c = addConst (C_CLASS (id2cname c))
  fun funInitCid c = addConst (C_MREF (id2cname c, "<init>",
    funInitDesc c))

  val funApplyCid  = addConst (C_IREF (funIname, "apply", funApplyDesc))

  fun getLoc l = Option.valOf (LMAP.find (locmap, l)) handle Option => (print
    "3"; NUL)
  fun fld2fref l = C_FREF (id2cname closid, id2fname l, objectDesc)

  (* add field to the class and give the map *)
  val fld2cidmap = List.foldl (fn (f, fcmap) => let
    val cid = addConst (fld2fref f) in
    addField ([JF.ACC_PUBLIC], id2fname f, objectDesc);
    IMAP.insert (fcmap, f, cid) end) (IMAP.empty) (getFlds closid)

  fun getFldCid f = valOf (IMAP.find (fld2cidmap, f)) 
    handle Option => (print (Int.toString f) ; 
    print ("--" ^ (Int.toString closid)); print "\n"; 0)


  (* move local variable or field to/from stack top *)
  fun l2s (LOC l)      = [ALOAD l]
    | l2s (FLD l) = [ALOAD_0, GETFIELD (getFldCid l)]

    | l2s NUL = [ACONST_N]
    | l2s (BAS i) = [GETSTATIC (addConst
        (C_FREF (basCname, id2fname i, objectDesc)))]

  fun s2l (LOC l) = ([], [ASTORE l])
    | s2l (FLD l) = ([ALOAD_0], [PUTFIELD (getFldCid l)])

    | s2l NUL = ([],[POP])
    | s2l (BAS _) = raise Size

  val loc2stk = l2s o getLoc

  fun stk2loc l exp = let
    val (prev, after) = (s2l o getLoc) l in
    prev @ exp @ after end

  val initCode = #1 (List.foldl (fn (f, (code, n)) =>
    (code @ [ALOAD_0, ALOAD n, PUTFIELD (getFldCid f)], n+1)
    ) ([], 1) (getFlds closid))

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

          | gen (NEWFCN (l, f))         = (let
          val fCid = funClsCid f
          val init = funInitCid f
          val tlocmap = Option.valOf (IMAP.find (glocmap, f))
          val loadparas = List.foldl (fn (fid, code) =>
            code @ (loc2stk (getExfloc f fid)) ) [] (getFlds f) in
          CODE (stk2loc l ([NEW fCid, DUP] @ loadparas @ [INVOKESPECIAL init])) end
          handle Option => (TIO.println "gen NEWFCN"; raise Option))

          | gen (NEWSCN (l, s))         = (case s of
              INT_SCON i => CODE
                (stk2loc l [BIPUSH i, INVOKESTATIC intValofCid]))

          | gen (NEWRCD (l, ls))        = let
          val (rcode, _) = List.foldl (fn (l, (c, i)) => let
            val lload = loc2stk l
            val kCid = addConst (C_INT i)
            val kload = [LDC kCid] in
            (c @ [DUP] @ kload @ lload @ [AASTORE], i + 1) end)
            ([], 0) ls
          val lenCid = addConst (C_INT (List.length ls))
          val code = [LDC lenCid, ANEWARRAY objectCid] @ rcode in
          CODE (stk2loc l code) end

          | gen (NEWTAG (l1, l2, c))    = let
          val l2load = loc2stk l2
          val code = [NEW tagClsCid, DUP, BIPUSH c] @
            l2load @ [INVOKESPECIAL tagInitCid] in
          CODE (stk2loc l1 code) end

          | gen (MATRCD (l1, l2, i)) = let
          val kCid = addConst (C_INT i)
          val (code0, code3) = (s2l o getLoc) l1
          val code = code0 @ (loc2stk l2) @ [LDC kCid, AALOAD] @ code3 in
            CODE code end

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

          | gen (RAISE l)   = CODE ([NEW packClsCid, DUP] @ (loc2stk l) @
            [INVOKESPECIAL packInitCid, ATHROW])

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
      (Option.valOf codeop) @ (gen is newoff) else (let
      val rest = gen is newoff
      val (codeop, _) = genInst i in
      (Option.valOf codeop) @ rest end
      handle Option => (TIO.println "gen LAST"; raise Option) )
    end end

  (* genCode *)
  in gen meth 0 end

  (* genClos *)
  in (case clos of
    TOP (_, _, meth) => let
    val code = [ALOAD_0, INVOKESPECIAL objInitCid] @ (genCode meth)
    val codeattr = JC.newAttr class (CODE code) in
    JC.addMethod class ([JM.ACC_PUBLIC], "<init>", initDesc, codeattr);
    class end
  | FCN (prev, _, _, meth) => let
    val prevCid = (prevCid closid prev)
    val initcodeattr = JC.newAttr class (CODE
      ([ALOAD_0, INVOKESPECIAL objInitCid] @ initCode @ [RETURN]))
    val code = genCode meth
    val codeattr = JC.newAttr class (CODE code) in
    JC.addMethod class ([JM.ACC_PUBLIC], "<init>", funInitDesc closid, initcodeattr);
    JC.addMethod class ([JM.ACC_PUBLIC], "apply", funApplyDesc, codeattr);
    class end) end

  (* genProg *)
  in IMAP.foldli (fn (id, c, l) =>
          (id, genClos (id, c)) :: l) [] prog end

  fun writeProg prog dir = let
    val clses = genProg prog in
    List.map (fn (id, c) => JC.write c
        (dir ^ "/" ^ "C" ^ (Int.toString id) ^ ".class")) clses; () end

end
