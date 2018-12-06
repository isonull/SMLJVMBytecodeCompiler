structure InitialStaticBasis = struct

  structure TY = Type
  structure TS = TypeScheme
  structure VS = VartySet
  structure TF = TypeFunction
  structure LM = LabBinaryMap
  structure SM = StringBinaryMap
  structure TNS = TypeNameBinarySet

  structure T = TypeNameEnvironment
  structure U = TypeVariableEnvironment

  datatype ty = datatype TY.ty
  datatype idstat = datatype IdStatus.idstat

  val unitTycon = "unit"
  val boolTycon = "bool"
  val intTycon  = "int"
  val wordTycon = "word"
  val realTycon = "real"
  val strTycon  = "string"
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

  val unitTysch = (VS.empty, TY.ROWTY (LM.empty))
  val boolTysch = TS.fromTyname boolTyname
  val intTysch  = TS.fromTyname intTyname
  val wordTysch = TS.fromTyname wordTyname
  val realTysch = TS.fromTyname realTyname
  val strTysch  = TS.fromTyname strTyname
  val charTysch = TS.fromTyname charTyname
  val listTysch = TS.fromTyname listTyname
  val refTysch  = TS.fromTyname refTyname
  val exnTysch  = TS.fromTyname exnTyname

  val unitTyfcn = ([], ROWTY (LM.empty))
  val boolTyfcn = TF.fromTyname boolTyname
  val intTyfcn  = TF.fromTyname intTyname
  val wordTyfcn = TF.fromTyname wordTyname
  val realTyfcn = TF.fromTyname realTyname
  val strTyfcn  = TF.fromTyname strTyname
  val charTyfcn = TF.fromTyname charTyname
  val listTyfcn = TF.fromTyname listTyname
  val refTyfcn  = TF.fromTyname refTyname
  val exnTyfcn  = TF.fromTyname exnTyname

  val boolValstr = (boolTysch, CON)
  val nilListValstr = (listTysch, CON)
  val conListValstr = ((VS.fromList [0],
    FUNTY (ROWTY (LM.fromList [VARTY 0, CONTY ([VARTY 0], listTyname)]),
           CONTY ([VARTY 0], listTyname))), CON)
  val refValstr = ((VS.fromList [0], FUNTY (VARTY 0, (CONTY ([VARTY 0], refTyname)))), CON)
  val exnValstr = (exnTysch, EXC)

  val unitValenv = SM.empty
  val intValenv = SM.empty
  val wordValenv = SM.empty
  val realValenv = SM.empty
  val strValenv = SM.empty
  val charValenv = SM.empty
  val exnValenv = SM.empty

  val boolValenv = SM.fromListPair
    [("true", boolValstr), ("false", boolValstr)]

  val listValenv = SM.fromListPair
    [("nil", nilListValstr), ("::", conListValstr)]

  val refValenv = SM.fromListPair [("ref", refValstr)]


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

  val tyenv = SM.fromListPair tyenvMapListPair

  val vidList = ["ref", "nil", "true", "false", "Match", "Bind",
    "::", "=", ":="]

  val refValstr   = refValstr
  val nilValstr   = nilListValstr
  val trueValstr  = boolValstr
  val falseValstr = boolValstr
  val matchValstr = exnValstr
  val bindValstr  = exnValstr
  val conValstr   = conListValstr
  val eqValstr    = ((VS.singleton 0,
    FUNTY (ROWTY (LM.fromList [VARTY 0, VARTY 0]),
           CONTY ([], boolTyname))), VAL)
  val signValstr  = ((VS.singleton 0,
    FUNTY (ROWTY (LM.fromList [CONTY ([VARTY 0], refTyname), VARTY 0]),
           ROWTY LM.empty)), VAL)

  val valstrList = [refValstr, nilValstr, trueValstr, falseValstr,
    matchValstr, bindValstr, conValstr, eqValstr, signValstr]

  val valenv = SM.fromListPair (ListPair.zip (vidList, valstrList))

  val strenv = SM.empty

  val env = Environment.ENV (strenv, tyenv, valenv)

  val tynameset = T.fromTynameset (TNS.fromList tynameList)

  val tyvarset = U.empty

  val context = (tynameset, tyvarset, env)

end
