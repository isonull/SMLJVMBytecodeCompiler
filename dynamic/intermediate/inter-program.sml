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

  fun getRefs prog = IM.foldl (fn (c, r) => r @ (IC.getRefs c)) [] prog

  fun getMethod prog c = IC.getMethod (valOf (IM.find (prog, c)))

  fun getSingleRefAddr prog = let
    val refs = getRefs prog in ListAux.uniqueElements refs end

  fun getMaxloc prog cid = IC.getMaxloc (valOf (find (prog, cid)))

  (*fun add prog f id closinfo = let*)
    (*val newclos = IC.new f closinfo in*)
    (*insert (prog, id, newclos) end*)

  fun mapMethod p f = IM.map (fn TOP (_, i, m) => f m
                              |  FCN (c, _, i, m) => f m) p

  fun toString prog = IM.toString prog (fn k => i2s k) IC.toString "" 
    "\n --------------------------- \n"
end

structure ClosureInline = struct

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

end
