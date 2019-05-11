structure InterProgram = struct

  structure IC = InterClosure
  structure IM = IntBinaryMapAux

  datatype closure = datatype IC.closure

  open IntBinaryMapAux

  (* closure graph for cross closure access *)
  type program = (closure map)
  val i2s = Int.toString

  val valOf = Option.valOf

  fun distance prog x y = (if x = y then
    SOME 0 else let
      val cx = valOf (find (prog, x)) in
      SOME ((valOf (distance prog (IC.prevClosid cx) y)) + 1) end)
    handle Option => NONE
         | NotFound => NONE

  fun relDistance prog x y= let
    val d1 = distance prog x y
    val d1 = Option.getOpt (d1, ~1)
    val d2 = distance prog y x
    val d2 = Option.getOpt (d2, ~1) in
    if d1 >= 0 then SOME d1
    else if d2 >= 0 then SOME (~d2)
    else NONE end

  fun path prog x y = (if x = y then
    SOME [x] else let
      val cx = valOf (find (prog, x))
      val xp = IC.prevClosid cx in
      SOME (x :: (valOf (path prog xp y))) end)
    handle Option => NONE
         | NotFound => NONE

  fun prev prog x = IC.prevClosid (valOf (find (prog, x)))
  fun next prog x = IC.nextClosid (valOf (find (prog, x)))

  fun updateClosure prog c m = let
    val clos = valOf (IM.find (prog, c))
      handle Option => (TIO.println "removeNext"; raise Option) in
    IM.insert (prog, c, m clos) end

  fun removeNext prog c n =
    updateClosure prog c (fn cl => IC.removeNext cl n)

  fun updateMethod prog c m =
    updateClosure prog c (fn cl => IC.updateMethod cl m)


  fun getMethod prog c = IC.getMethod (valOf (IM.find (prog, c)))

  fun getMaxloc prog cid = IC.getMaxloc (valOf (find (prog, cid)))

  (*fun add prog f id closinfo = let*)
    (*val newclos = IC.new f closinfo in*)
    (*insert (prog, id, newclos) end*)

  fun mapMethod p f = IM.map (fn TOP (_, i, m) => f m
                              |  FCN (c, _, i, m) => f m) p

  fun toString prog = IM.toString prog (fn k => i2s k) IC.toString ""
    "\n --------------------------- \n"
end

(* structure ClosureInline = struct

  structure IM = InterMethod
  structure IP = InterProgram
  structure IC = InterClosure
  structure II = InterInstruction
  structure LSET = LocationBinarySet
  structure LMAP = LocationBinaryMap
  datatype code = datatype InterInstruction.code

  val isSome = Option.isSome
  val valOf = Option.valOf

  fun isCallAddr (CALL (l1, l2, l3)) addr = l2 = addr
    | isCallAddr _ _ = false

  exception Goto

  val prog = ref (IntBinaryMapAux.empty : IP.program)

  fun closureGraphVerify f c = (
    TIO.println "CHECK graph";
    TIO.println (Int.toString f);
    TIO.println (Int.toString c);
    (Option.getOpt (IP.relDistance (! prog) f c, ~1) = 1) andalso
    (List.null (IP.next (! prog) f))
    )

  fun singleReferenceVerify a = (
    TIO.println "CHECK single ref";
    TIO.println (ListAux.toString
      (IP.getSingleRefAddr (! prog))
      Location.toString ",");
    ListAux.member (IP.getSingleRefAddr (! prog), a)
    )

  fun verify f c a = (
    TIO.println "CHECK";
    (singleReferenceVerify a) andalso (closureGraphVerify f c))

  fun noSideBr m pc = if pc = 0 then false else let
    val inst = List.nth (m, pc - 1) in
    case inst of
      RETURN _ => true
    | RAISE _ => true
    | _ => false end

  fun nextPc m pc = let
    val curr = List.nth (m, pc) in
    case curr of
      GOTO l => (let val labpc = IM.getLabelIndex m l in
      if noSideBr m (valOf labpc) then labpc else NONE end
      handle Option => NONE)
    | RETURN l => NONE
    | RAISE _ => NONE
    | i =>  if List.null (II.br i) then
              if (pc + 1) < List.length m then
                SOME (pc + 1) else NONE else
              NONE end

  fun findOpt prog = let

    fun aux prog cid method = let
      val res = ref NONE in (
      List.foldl (fn
      (NEWFCN (addr, f), i) => let
        val pc = ref (SOME i) in
        while (isSome (! pc)) andalso
              (not (isCallAddr (List.nth (method, valOf (! pc))) addr)) do
          pc := nextPc method (valOf (! pc));
        if isSome (! pc) then
          if (verify f cid addr) then let
            val (CALL (outaddr, _, inaddr)) = List.nth (method, valOf (! pc)) in
            (res := SOME (f, i, valOf (! pc), inaddr, outaddr); raise Goto) end else
            (i+1) else
          (i+1) end
      | (_, i) => i+1) 0 method;
        NONE ) handle Goto => ! res end
      val res = ref NONE in

    (IntBinaryMapAux.mapi ( fn (cid, clos) =>
    let val r = aux prog cid (IC.getMethod clos) in
      if isSome r then
        (res := SOME (cid, (valOf r)); raise Goto) else () end) prog;
      NONE)
      handle Goto => ! res end

  structure ICT = IntCounter
  exception Fail

  val locct = ref 0
  val labct = ref 0
  val cid = ref 0
  val fid = ref 0
  val cpc = ref 0
  val fpc = ref 0
  val inaddr = ref (0, 0)
  val outaddr = ref (0, 0)
  val reldis = ref 0

  fun getLocmap method = let
    val locs = LSET.fromList (IM.getLocs method)
    val _ = TIO.println (LSET.toString locs );
    val locmap = LSET.foldl (fn ((c, l), map) => let
      val reldis = IP.relDistance (! prog) c (! cid) in
      if (getOpt (reldis, ~1)) = 1 then
        if l > 0 then
          LMAP.insert (map, (c, l), (! cid, ICT.next locct))
        else if l = ~1 then
          LMAP.insert (map, (c, l), ! inaddr)
        else raise Fail
      else if c = ~1 then map
      else raise Fail end) LMAP.empty locs in locmap end

  fun getRetmap method  = let
    val retlocs = LSET.fromList (IM.getRetlocs method)
    val _ = TIO.println (LSET.toString retlocs );
    val locmap = LSET.foldl (fn ((c, l), map) => let
      val reldis = IP.relDistance (! prog) c (! cid) in
      if (getOpt (reldis, ~1)) = 1 then
        if l > 0 then
          LMAP.insert (map, (c, l), ! outaddr)
        else if l = ~1 then
          map
        else raise Fail
      else if c = ~1 then map
      else raise Fail end) LMAP.empty retlocs in locmap end

  val lmap : (((int * int) LMAP.map) ref) = ref LMAP.empty
  val rmap : (((int * int) LMAP.map) ref) = ref LMAP.empty
  val retlab = ref 0

  fun optF m = case m of
      i :: m => (case i of
        RETURN a => [
          MOV  (valOf (LMAP.find (!rmap, a)),
                valOf (LMAP.find (!lmap, a))),
          GOTO (! retlab)] @ (optF m)
      | _ => (II.replaceLocs (!lmap) i) :: (optF m) )
    | [] => [LABEL (! retlab)]

  fun optC (i :: m) fm pc = let val rest = optC m fm (pc + 1) in
    if pc = (! fpc)      then rest
    else if pc = (! cpc) then (optF fm) @ rest
    else                  i :: rest
  end
    | optC [] _ _ = []

  fun opt (p, l) = (
    prog := p;
    labct := l;
    let val optpt = findOpt (! prog) in
      (case optpt of NONE => ()
      | SOME (cidi, (fidi, fpci, cpci, inaddri, outaddri)) => (
        TIO.println ("VERFY PASS");
        cid := cidi;
        fid := fidi;
        fpc := fpci;
        cpc := cpci;
        inaddr := inaddri;
        outaddr := outaddri;
        locct := IP.getMaxloc (! prog) cidi));
        retlab := ICT.next labct end;
    lmap := getLocmap (IP.getMethod (! prog) (! fid));
    TIO.println (LMAP.toString (! lmap) );
    rmap := getRetmap (IP.getMethod (! prog) (! fid));
    TIO.println (LMAP.toString (! rmap) ); let
      val methf = optC (IP.getMethod (! prog) (! cid))
          (IP.getMethod (! prog) (! fid)) 0
      val reldis = valOf (IP.relDistance (! prog) (! fid) (! cid))
        handle Option => (TIO.println "xxx"; raise Option) in
      if reldis = 1 then
        TIO.println (IM.toString methf)  else () end;
    (! prog)
    )

end *)

structure InterAnalysis = struct

  structure II = InterInstruction
  structure IC = InterClosure
  structure IM = InterMethod
  structure IP = InterProgram

  structure IMAP = IntBinaryMapAux
  structure LSET = LocationBinarySet
  datatype code = datatype II.code

  fun getFcnRefs prog f = let

    val nonlocset = ref ((IMAP.map (fn _ => LSET.empty) prog)
    : (LSET.set) IMAP.map)

    fun addls cid (loc as (c, l)) = let
      val lm = ! nonlocset
      val clm = valOf (IMAP.find (lm, cid))
        handle Option => (TIO.println "addls-clm"; raise Option)
      val psop = LSET.member (clm, loc) in
      if psop orelse c = ~1 orelse c = cid then () else let
        val newclm = LSET.add (clm, loc)
        val newlm  = IMAP.insert (lm, cid, newclm) in
      nonlocset := newlm end end

  fun genClos (cid, clos) = let

  fun genMeth meth = let

    val locs = IM.getLocs meth in

    (* genMeth *)
    List.map (fn (c, l) => let
      val path = valOf (IP.path prog cid c)
        handle Option => (TIO.println "genMeth-path";
        TIO.println (Int.toString cid);
        TIO.println (Int.toString c);
        raise Option) in
      List.map (fn cid => addls cid (c, l)) path end) locs end in

    (* genClos *)
    genMeth (case clos of
                  IC.TOP (_, _, m) => m
                | IC.FCN (_, _, _, m) => m) end in
    (* genProg *)
    IMAP.mapi (fn p => genClos p) prog;
    valOf (IMAP.find (! nonlocset, f)) end
    handle Option => LSET.empty

  fun getRefs prog (MOV    (l1, l2))       = LSET.fromList [l2]
    | getRefs prog (NEWFCN (l, f))         = getFcnRefs prog f
    | getRefs prog (NEWSCN (l, s))         = LSET.fromList []
    | getRefs prog (NEWRCD (l, ls))        = LSET.fromList []
    | getRefs prog (NEWTAG (l1, l2, c))    = LSET.fromList [l2]
    | getRefs prog (MATRCD (l1, l2, l))    = LSET.fromList [l2]
    | getRefs prog (MATTAG (l1, l2, c, b)) = LSET.fromList [l2]
    | getRefs prog (MATINT (l, i, b))      = LSET.fromList []
    | getRefs prog (CALL   (l1, l2, l3))   = LSET.fromList [l2, l3]

    | getRefs prog (RAISE   l)   =           LSET.fromList [l]

    | getRefs prog (LABEL  l)              = LSET.fromList []
    | getRefs prog (GOTO l)                = LSET.fromList []
    | getRefs prog (RETURN l)              = LSET.fromList [l]
    | getRefs prog EXIT                    = LSET.fromList []

  fun getDefs (MOV    (l1, l2))       = LSET.fromList [l1]
    | getDefs (NEWFCN (l, f))         = LSET.fromList [l]
    | getDefs (NEWSCN (l, s))         = LSET.fromList [l]
    | getDefs (NEWRCD (l, ls))        = LSET.fromList [l]
    | getDefs (NEWTAG (l1, l2, c))    = LSET.fromList [l1]
    | getDefs (MATRCD (l1, l2, l))    = LSET.fromList [l1]
    | getDefs (MATTAG (l1, l2, c, b)) = LSET.fromList [l1]
    | getDefs (MATINT (l, i, b))      = LSET.fromList [l]
    | getDefs (CALL   (l1, l2, l3))   = LSET.fromList [l1]

    | getDefs (RAISE   l)             = LSET.fromList [l]

    | getDefs (LABEL  l)              = LSET.fromList []
    | getDefs (GOTO l)                = LSET.fromList []
    | getDefs (RETURN l)              = LSET.fromList []
    | getDefs EXIT                    = LSET.fromList []

  fun getDep prog i1 i2 = let
    val def1 = getDefs i1
    val def2 = getDefs i2
    val ref1 = getRefs prog i1
    val ref2 = getRefs prog i2
    val d1r2 = LSET.isEmpty (LSET.intersection (def1, ref2))
    val d2r1 = LSET.isEmpty (LSET.intersection (def2, ref1))
    val d1d2 = LSET.isEmpty (LSET.intersection (def1, def2)) in
    d1r2 orelse d2r1 orelse d1d2 end

end

structure TailCall = struct
  structure IM = InterMethod
  structure IP = InterProgram
  structure IC = InterClosure
  structure II = InterInstruction
  structure LSET = LocationBinarySet
  structure LMAP = LocationBinaryMap
  structure IMAP = IntBinaryMapAux
  structure ICT = IntCounter

  datatype code = datatype InterInstruction.code

  val isSome = Option.isSome
  val valOf = Option.valOf
  fun opt prog lab = let

    val labct = ref lab
    fun isRecCall (CALL (_, (_, 0), _)) = true
      | isRecCall _  = false

    fun getRecloc (CALL (_, l, _)) = l

    fun trackCall meth pc recloc = let
      val pc = ref pc
      val pctr = ref ([] : int list)
      val curloc = ref recloc
      val cont = ref true in
      while (! cont) do let
        val inst = List.nth (meth, ! pc) in
        case inst of
          CALL (r, f, a) =>
          if f = (! curloc) then (
            pctr := (! pc) :: (! pctr);
            pc := (! pc) + 1;
            curloc := r)
          else (
            curloc := r;
            pc := (! pc) + 1;
            cont := false)
        | _ => (cont := false)
        end ;
      (rev (! pctr), ! pc, ! curloc)
      end

    fun trackRet meth pc retloc = let
      val pc = ref pc
      val curloc = ref retloc
      val cont = ref true
      val res = ref false in
      while (! cont) do let
        val inst = List.nth (meth, ! pc) in
        case inst of
          MOV (r, d) =>
          if d = (! curloc) then (
            pc := (! pc) + 1;
            curloc := r)
          else (
            cont := false;
            res := false)
        | GOTO l => let val pcop = IM.getLabelIndex meth l in
          if isSome pcop then pc := valOf pcop
          else (
            cont := false;
            res := false) end
        | RETURN r =>  (
          cont := false;
          res := r = (! curloc))
        | LABEL l => pc := (! pc) + 1
        | _ => cont := false
        end ;
      (! res)
      end

    exception TransFail

    fun trackSinkFcn meth sink = case meth of
        [MOV ((c,~1),d), NEWFCN (x, f), MOV(r, y), GOTO l1, LABEL l2, LABEL l3] =>
        if l1 = l3 andalso x = y andalso r = sink then 
          SOME (f, [MOV ((c,~1),d)]) else NONE
      | [MOV ((c,~1),d), NEWFCN (x, f), MOV(r, y), GOTO l1, LABEL l3] =>
        if l1 = l3 andalso x = y andalso r = sink then 
          SOME (f, [MOV ((c,~1),d)]) else NONE
      | _ => NONE

    fun trans cid lab pcs = let
      val locct = ref (IP.getMaxloc prog cid)
      val _ = TIO.println (ListAux.toString pcs Int.toString ",")
      val slab  = ICT.next labct
      val meth  = IP.getMethod prog cid
      fun genBlock fid dloc rloc = let
        val retlab = ICT.next labct
        val (methf, newlabmax) = IM.newLabel (IP.getMethod prog fid) (! labct)
        val _ = labct := newlabmax
        fun proc ((RETURN a) :: m) =
          [MOV (rloc, a), GOTO retlab] @ (proc m)
          | proc (i :: m) = i :: (proc m)
          | proc [] = [] in
        [MOV ((fid, ~1), dloc)] @ (proc methf) @ [LABEL retlab] end
      val (replaceMap, _) = List.foldl (fn (pc, (code, f')) => case List.nth (meth, pc) of
          (CALL (r, (f, 0), d)) =>
          if f = cid then
            (IMAP.insert (code, pc, [MOV ((cid, ~1), d), GOTO slab]), ~2)
          else let
            val block = genBlock f d r
            val sinkfid = trackSinkFcn block r in
            if isSome sinkfid then let
              val (fid, block) = valOf sinkfid in
              (IMAP.insert (code, pc, block), fid) end
            else raise TransFail end
        | (CALL (r, f, d)) =>
          if f' = cid then
            (IMAP.insert (code, pc, [MOV ((cid, ~1), d), GOTO slab]), ~2)
          else let
            val block = genBlock f' d r
            val sinkfid = trackSinkFcn block r in
            if isSome sinkfid then let 
              val (fid, block) = valOf sinkfid in
              (IMAP.insert (code, pc, block), fid) end
            else raise TransFail end
          ) (IMAP.empty, ~2) pcs
      val (code, _) = List.foldl (fn (inst, (code, pc)) => let
        val repop = IMAP.find (replaceMap, pc) in
        if isSome repop then (code @ (valOf repop), pc+1)
        else (code @ [inst], pc+1) end) ([], 0) meth in
      (LABEL slab) :: code end

    fun optOnce prog = IP.foldli (fn (cid, clos, newprog) => 
      if isSome newprog then newprog else let
      val meth = IC.getMethod clos
      val (_, newmeth) = List.foldl ( fn (inst, (pc, newmeth)) => 
        if not (isSome newmeth) then

          if isRecCall inst then (let
            val (pcs, endpc, endloc) = trackCall meth pc (getRecloc inst)
            val succ = trackRet meth endpc endloc in
            if succ then let
              val newmeth = trans cid lab pcs in
              TIO.println "SUCC";
              (pc+1, SOME newmeth) end 

            else (pc + 1, newmeth) end)
          else (pc + 1, newmeth)

        else (pc + 1, newmeth)) (0, NONE) meth in 
      if isSome newmeth then 
        SOME (IP.updateMethod prog cid (valOf newmeth)) 
      else newprog end) NONE prog in let
    val progop = optOnce prog in 
    if isSome progop then (opt (valOf progop) (! labct)) 
    else (prog, ! labct) end end

end

structure InterOptimise = struct 
  
  fun opt prog lab = #1 (TailCall.opt prog lab)
end
