structure InitialStaticBasis : INITIAL_STATIC_BASIS = struct

  open Type
  open StaticBasis
  open Identifier

  val unitTycon = "unit"
  val boolTycon = "bool"
  val intTycon  = "int"
  val wordTycon = "word"
  val realTycon = "real"
  val strTycon  = "str"
  val charTycon = "char"
  val listTycon = "list"
  val refTycon  = "ref"
  val exnTycon  = "exn"

  val unitTyname = (([],unitTycon), 0, true)
  val boolTyname = (([],boolTycon), 0, true)
  val intTyname  = (([],intTycon ), 0, true)
  val wordTyname = (([],wordTycon), 0, true)
  val realTyname = (([],realTycon), 0, false)
  val strTyname  = (([],strTycon ), 0, true)
  val charTyname = (([],charTycon), 0, true)
  val listTyname = (([],listTycon), 1, true)
  val refTyname  = (([],refTycon ), 1, true)
  val exnTyname  = (([],exnTycon ), 0, false)

  (* type of value constructor *)
  (*val listTy = CONTY ([VARTY "a"], listTyname)*)
  (*val refTy = CONTY ([VARTY 1], refTyname)*)

  val unitTy = tynameInTy unitTyname
  val boolTy = tynameInTy boolTyname
  val intTy  = tynameInTy intTyname
  val wordTy = tynameInTy wordTyname
  val realTy = tynameInTy realTyname
  val strTy  = tynameInTy strTyname
  val charTy = tynameInTy charTyname
  val listTy = tynameInTy listTyname
  val refTy  = tynameInTy refTyname
  val exnTy  = tynameInTy exnTyname

  val unitTysch = ([], ROWTY (LBM.empty))
  val boolTysch = tynameInTysch boolTyname
  val intTysch  = tynameInTysch intTyname
  val wordTysch = tynameInTysch wordTyname
  val realTysch = tynameInTysch realTyname
  val strTysch  = tynameInTysch strTyname
  val charTysch = tynameInTysch charTyname
  val listTysch = tynameInTysch listTyname
  val refTysch  = tynameInTysch refTyname
  val exnTysch  = tynameInTysch exnTyname

  val unitTyfcn = ([], ROWTY (LBM.empty))
  val boolTyfcn = tynameInTyfcn boolTyname
  val intTyfcn  = tynameInTyfcn intTyname
  val wordTyfcn = tynameInTyfcn wordTyname
  val realTyfcn = tynameInTyfcn realTyname
  val strTyfcn  = tynameInTyfcn strTyname
  val charTyfcn = tynameInTyfcn charTyname
  val listTyfcn = tynameInTyfcn listTyname
  val refTyfcn  = tynameInTyfcn refTyname
  val exnTyfcn  = tynameInTyfcn exnTyname

  val conListRowty = ROWTY (LBM.insert ((LBM.insert
    (LBM.empty, INTLAB 1, VARTY "a")), INTLAB 2, listTy))

  (* type structure of value constructor *)

  val boolValstr : valstr= (([],CONTY ([], boolTyname)), CON)
  val nilListValstr = ((["a"], listTy), CON)
  val conListValstr = ((["a"], FUNTY (conListRowty, listTy)), CON)
  val refValstr = ((["a"], FUNTY (VARTY "a", refTy)), CON)
  val exnValstr = (tynameInTysch exnTyname, EXC)

  (* value environment of tycon *)

  val unitValenv = SBM.empty
  val intValenv = SBM.empty
  val wordValenv = SBM.empty
  val realValenv = SBM.empty
  val strValenv = SBM.empty
  val charValenv = SBM.empty
  val exnValenv = SBM.empty
  
  val boolValenv = SBM.fromListPair 
    [("true", boolValstr), ("false", boolValstr)]

  val listValenv = SBM.fromListPair
    [("nil", nilListValstr), ("::", conListValstr)]

  val refValenv = SBM.fromListPair [("ref", refValstr)]

  (* construct tyenv*)

  val tynameList = [unitTyname, boolTyname, intTyname, wordTyname,
    realTyname, strTyname, charTyname, listTyname, refTyname, exnTyname]

  val tyfcnList = [unitTyfcn, boolTyfcn, intTyfcn, wordTyfcn,
    realTyfcn, strTyfcn, charTyfcn, listTyfcn, refTyfcn, exnTyfcn]

  val tyconList = [unitTycon, boolTycon, intTycon, wordTycon,
    realTycon, strTycon, charTycon, listTycon, refTycon, exnTycon]

  val valenvList = [unitValenv, boolValenv, intValenv, wordValenv,
    realValenv, strValenv, charValenv, listValenv, refValenv, exnValenv]

  val tyfcnValenvList = ListPair.zip (tyfcnList, valenvList)
  val tyenvMapListPair = ListPair.zip (tyconList, tyfcnValenvList)

  val tyenv = SBM.fromListPair tyenvMapListPair

  (* construct valenv *)

  val vidList = [ "ref", "nil", "true", "false", "Match", "Bind"]

  val refValstr   = refValstr
  val nilValstr   = nilListValstr
  val trueValstr  = boolValstr
  val falseValstr = boolValstr
  val matchValstr = exnValstr
  val bindValstr  = exnValstr

  val valstrList = [refValstr, nilValstr, trueValstr,
    falseValstr, matchValstr, bindValstr]

  val valenv = SBM.fromListPair (ListPair.zip (vidList, valstrList))

  val strenv = SBM.empty

  val env = ENV (strenv, tyenv, valenv)

  val tynameset = TBS.fromList tynameList

  val tyvarset = Type.emptyU

  val asstyset = Type.emptyA

  val context = (tynameset, tyvarset, asstyset, env)
end

structure ISB = InitialStaticBasis
