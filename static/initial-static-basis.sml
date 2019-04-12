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
  datatype idstat = datatype IdentifierStatus.idstat

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

  val wildUnitTysch = (VS.empty, TY.ROWTY (LM.empty, true))
  val unitTysch = (VS.empty, TY.ROWTY (LM.empty, false))
  val boolTysch = TS.fromTyname boolTyname
  val intTysch  = TS.fromTyname intTyname
  val wordTysch = TS.fromTyname wordTyname
  val realTysch = TS.fromTyname realTyname
  val strTysch  = TS.fromTyname strTyname
  val charTysch = TS.fromTyname charTyname
  val listTysch = TS.fromTyname listTyname
  val refTysch  = TS.fromTyname refTyname
  val exnTysch  = TS.fromTyname exnTyname

  val unitTyfcn = ([], ROWTY (LM.empty, false))
  val boolTyfcn = TF.fromTyname boolTyname
  val intTyfcn  = TF.fromTyname intTyname
  val wordTyfcn = TF.fromTyname wordTyname
  val realTyfcn = TF.fromTyname realTyname
  val strTyfcn  = TF.fromTyname strTyname
  val charTyfcn = TF.fromTyname charTyname
  val listTyfcn = TF.fromTyname listTyname
  val refTyfcn  = TF.fromTyname refTyname
  val exnTyfcn  = TF.fromTyname exnTyname

  val intTy = CONTY ([], intTyname)
  val strTy = CONTY ([], strTyname)
  val intpairTy = ROWTY (LM.fromList [intTy, intTy], false)

  val isubTysch = (VS.empty, FUNTY (intpairTy, intTy))
  val iaddTysch = (VS.empty, FUNTY (intpairTy, intTy))
  val imulTysch = (VS.empty, FUNTY (intpairTy, intTy))
  val idivTysch = (VS.empty, FUNTY (intpairTy, intTy))
  val iremTysch = (VS.empty, FUNTY (intpairTy, intTy))
  val inegTysch = (VS.empty, FUNTY (intTy, intTy))
  val tostrTysch= (VS.fromList [(0, false)], 
    FUNTY (VARTY (0, false), strTy))
  val printTysch= (VS.empty, FUNTY (strTy, TY.ROWTY (LM.empty, false)))
  val drefValstr= ((VS.fromList [(0, false)],
    FUNTY ((CONTY ([VARTY (0, false)], refTyname)),
            VARTY (0, false))), CON)
  val arefValstr= ((VS.fromList [(0, false)],
    FUNTY (ROWTY (LM.fromList [CONTY ([VARTY (0, false)], refTyname), 
                               VARTY (0, false)], false),
           CONTY ([VARTY (0, false)], listTyname))), CON)

  val boolValstr = (boolTysch, CON)
  val nilValstr = (listTysch, CON)
  val conValstr = ((VS.fromList [(0, false)],
    FUNTY (ROWTY (LM.fromList [VARTY (0, false), 
                               CONTY ([VARTY (0, false)], listTyname)], false),
           CONTY ([VARTY (0, false)], listTyname))), CON)
  val refValstr = ((VS.fromList [(0, false)],
    FUNTY (VARTY (0, false), 
          (CONTY ([VARTY (0, false)], refTyname)))), VAL)
  val exnValstr = (exnTysch, EXC)

  val isubValstr = (isubTysch, VAL)
  val iaddValstr = (iaddTysch, VAL)
  val imulValstr = (imulTysch, VAL)
  val idivValstr = (idivTysch, VAL)
  val iremValstr = (iremTysch, VAL)
  val inegValstr = (inegTysch, VAL)
  val tostrValstr= (tostrTysch, VAL)
  val printValstr= (printTysch, VAL)

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
    [("nil", nilValstr), ("con", conValstr)]

  val refValenv = SM.empty


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

  val refValstr   = refValstr
  val nilValstr   = nilValstr
  val trueValstr  = boolValstr
  val falseValstr = boolValstr
  val matchValstr = exnValstr
  val bindValstr  = exnValstr
  val conValstr   = conValstr
  val eqValstr    = ((VS.singleton (0, true),
    FUNTY (ROWTY (LM.fromList [VARTY (0, true), VARTY (0, true)], false),
           CONTY ([], boolTyname))), VAL)
  val signValstr  = ((VS.singleton (0, false),
    FUNTY (ROWTY (LM.fromList [CONTY ([VARTY (0, false)], refTyname), VARTY (0,
                                      false)], false),
           ROWTY (LM.empty, false))), VAL)

  val vidValstrListPair = [
    ("iadd"   , iaddValstr),
    ("isub"   , isubValstr),
    ("imul"   , imulValstr),
    ("idiv"   , idivValstr),
    ("irem"   , iremValstr),
    ("ineg"   , inegValstr),
    ("false"  , falseValstr),
    ("true"   , trueValstr),
    ("CON"    , conValstr),
    ("NIL"    , nilValstr),
    ("Match"  , matchValstr),
    ("Bind"   , bindValstr),
    ("ref"    , refValstr),
    ("dref"   , drefValstr),
    ("aref"   , arefValstr),
    ("eq"     , eqValstr),
    ("print"  , printValstr),
    ("tostr"  , tostrValstr)
    ]

  val valenv = SM.fromListPair vidValstrListPair

  val strenv = SM.empty

  val env = Environment.ENV (strenv, tyenv, valenv)

  val tynameset = T.fromTynameset (TNS.fromList tynameList)

  val tyvarset = U.empty

  val context = (tynameset, tyvarset, env)

end
