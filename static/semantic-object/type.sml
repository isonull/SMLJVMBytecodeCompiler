structure Type = struct

  open TypeName

  structure LS = LabBinarySet
  structure LM = LabBinaryMap
  structure IS = IntBinarySetAux
  structure LA = ListAux
  structure LPA = ListPairAux
  structure TN = TypeName
  structure VS = VartySet
  structure AS = IntBinarySetAux
  structure IM = IntBinaryMapAux

  datatype ty =
    VARTY of varty |
    ROWTY of rowty |
    FUNTY of funty |
    CONTY of conty |
    ASSTY of assty
  withtype rowty = (ty LM.map) * bool (* isWild *)
  and funty = ty * ty
  and tyseq = ty list
  and conty = tyseq * tyname
  and assty = int
  and varty = Varty.varty

  type sub = ty * ty
  type ins = assty * ty
  type bnd = varty * ty
  type subseq = sub list
  type insseq = ins list
  type bndseq = bnd list
  type vartyset = VS.set

  exception WrongTypeForm of string
  exception UnifyFail of string

  fun noWildRowty (VARTY _) = true
    | noWildRowty (ASSTY _) = true
    | noWildRowty (FUNTY (a, b)) = noWildRowty a andalso noWildRowty b
    | noWildRowty (CONTY (ts, n)) = List.all (fn t => noWildRowty t) ts
    | noWildRowty (ROWTY (r, w)) = (not w) andalso 
    List.all (fn t => noWildRowty t) (LM.listItems r)

  fun isVarty (VARTY _) = true
    | isVarty _ = false

  fun isAssty (ASSTY _) = true
    | isAssty _ = false

  fun toString (VARTY v) = Varty.toString v
    | toString (ROWTY (r, w)) =
    "{" ^ (LM.toString r Lab.toString toString "=" ",") ^ "}" ^
    (if w then "..." else "")
    | toString (CONTY (ts, n)) = let
      val vs = (LA.toString ts toString ",")
      val vs = if vs = "" then "" else vs ^ " " in
       vs ^ (TN.toString n) end
    | toString (FUNTY (t1, t2)) = "(" ^ (toString t1) ^ "->" ^ (toString t2) ^ ")"
    | toString (ASSTY t) = Assty.toString t

  fun printWrongTypeForm t = let
    val st = toString t in
    print ("WRONG TYPE FORM \n" ^ st) end

  fun getVartyset (VARTY v) = VS.singleton v
    | getVartyset (ROWTY (r, _)) =
    LM.foldl (fn (t, s) => VS.union ((getVartyset t), s)) VS.empty r
    | getVartyset (FUNTY (t1, t2)) =
    VS.union ((getVartyset t1), (getVartyset t2))
    | getVartyset (CONTY (ts, n)) =
    List.foldl (fn (t, s) => VS.union ((getVartyset t), s)) VS.empty ts
    | getVartyset (ASSTY _) = VS.empty

  fun getAsstyset (VARTY _) = AS.empty
    | getAsstyset (ROWTY (r, _)) =
    LM.foldl (fn (t, s) => AS.union ((getAsstyset t), s)) AS.empty r
    | getAsstyset (FUNTY (t1, t2)) =
    AS.union ((getAsstyset t1), (getAsstyset t2))
    | getAsstyset (CONTY (ts, n)) =
    List.foldl (fn (t, s) => AS.union ((getAsstyset t), s)) AS.empty ts
    | getAsstyset (ASSTY a) = AS.singleton a

  fun sub (VARTY vt) (VARTY vt', ty) = if vt' = vt then ty else (VARTY vt)
    | sub (VARTY v) (ASSTY a, _) = VARTY v
    | sub (ASSTY a) (ASSTY a', ty) = if a' = a then ty else (ASSTY a)
    | sub (ASSTY a) (VARTY v, _) = ASSTY a
    | sub (ROWTY (rowty, w)) s = ROWTY (LM.map (fn ty => sub ty s) rowty, w)
    | sub (FUNTY (argty, resty)) s = FUNTY (sub argty s, sub resty s)
    | sub (CONTY (tyseq, tyname)) s =
    CONTY (List.map (fn ty => sub ty s) tyseq, tyname)
    | sub t1 (t2, t3) = raise WrongTypeForm "WRONG SUB FORM"

  fun ins (VARTY vt) _ = VARTY vt
    | ins (ASSTY at) (at', ty) = if at' = at then ty else (ASSTY at)
    | ins (ROWTY (rowty, w)) i = ROWTY (LM.map (fn ty => ins ty i) rowty, w)
    | ins (FUNTY (argty, resty)) i = FUNTY (ins argty i, ins resty i)
    | ins (CONTY (tyseq, tyname)) i =
    CONTY (List.map (fn ty => ins ty i) tyseq, tyname)

  fun bnd (VARTY vt) (vt', ty) = if vt' = vt then ty else (VARTY vt)
    | bnd (ASSTY a) _ = ASSTY a
    | bnd (ROWTY (rowty, w)) s = ROWTY (LM.map (fn ty => bnd ty s) rowty, w)
    | bnd (FUNTY (argty, resty)) s = FUNTY (bnd argty s, bnd resty s)
    | bnd (CONTY (tyseq, tyname)) s =
    CONTY (List.map (fn ty => bnd ty s) tyseq, tyname)

  (* substitute the varty in an order *)
  fun substitute ty subseq = List.foldl (fn (s, ty) => sub ty s) ty subseq

  (* substitute the assty in an order *)
  fun instantiate ty insseq = List.foldl (fn (i, ty) => ins ty i) ty insseq

  (* check loop *)
  fun mapInstantiate (VARTY vt) im = VARTY vt
    | mapInstantiate (ASSTY a) im = let
    val insop = IM.find (im, a) in
    if Option.isSome insop then let 
      val ins = Option.valOf insop in 
      mapInstantiate ins im end else 
      (ASSTY a) end
    | mapInstantiate (ROWTY (rowty,w)) im = 
    ROWTY (LM.map (fn ty => mapInstantiate ty im) rowty, w)
    | mapInstantiate (FUNTY (argty, resty)) im =
    FUNTY (mapInstantiate argty im, mapInstantiate resty im)
    | mapInstantiate (CONTY (tyseq, tyname)) im =
    CONTY (List.map (fn ty => mapInstantiate ty im) tyseq, tyname)

  fun bind ty bndseq = List.foldl (fn (b, ty) => bnd ty b) ty bndseq

  fun isBnd (VARTY _, _) = true
    | isBnd (ASSTY _, _) = false
    | isBnd (_, _) = raise WrongTypeForm "WRONG SUB FORM"

  fun isIns (VARTY _, _) = false
    | isIns (ASSTY _, _) = true
    | isIns (_, _) = raise WrongTypeForm "WRONG SUB FORM"

  fun bndseqFromSubseq subs = let
    val sbnd = List.filter isBnd subs
    fun aux (VARTY a, b) = (a, b)
      | aux (_, _) = raise WrongTypeForm "WRONG SUB FORM" in
    map aux sbnd end

  fun insseqFromSubseq subs = let
    val sins = List.filter isIns subs
    fun aux (ASSTY a, b) = (a, b)
      | aux (_, _) = raise WrongTypeForm "WRONG SUB FORM" in
    map aux sins end

  (* check a type contains a varty or assty *)
  fun ctva (VARTY v') (VARTY v) = v = v'
    | ctva (ASSTY _) (VARTY _) = false
    | ctva (VARTY _) (ASSTY _) = false
    | ctva (ASSTY a') (ASSTY a) = a = a'
    | ctva (ROWTY (r, w)) v =
    LM.foldl (fn (t, b) =>  b orelse (ctva t v)) false r
    | ctva (CONTY (ts, n)) v =
    List.foldl (fn (t ,b) => b orelse (ctva t v)) false ts
    | ctva (FUNTY (t1, t2)) v = (ctva t1 v) orelse (ctva t2 v)
    | ctva _ _ = raise WrongTypeForm "WRONG CTVA FORM"

end
