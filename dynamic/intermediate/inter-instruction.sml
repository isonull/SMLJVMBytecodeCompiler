structure InterInstruction = struct

  structure SC = SpecialConstant
  structure LMAP = LocationBinaryMap

  type loc = int * int
  type fid = int

  datatype scon = datatype SpecialConstant.scon

  val etagBind  = 0
  val etagMatch = 1

  datatype code =
    MOV    of loc * loc |

    NEWFCN of loc * fid |
    NEWSCN of loc * scon |
    NEWRCD of loc * (loc list) |
    NEWTAG of loc * loc * int |

    MATRCD of loc * loc * int |
    MATTAG of loc * loc * int * label |
    MATINT of loc * int * label |

    CALL   of loc * loc * loc |
    RAISE  of loc |

    LABEL  of label |
    GOTO   of label |

    (*EXSTR  of int |*)
    (*EXEND  of int |*)
    (*HDEND  of int |*)

    RETURN of loc |
    EXIT

  withtype label = int
  and instruction = code

  val i2s = Int.toString

  fun l2s (a, b) = "(" ^ (i2s a) ^ "," ^ (i2s b) ^ ")"

  fun br (MOV    _)            = []
    | br (NEWFCN _)            = []
    | br (NEWSCN _)            = []
    | br (NEWRCD _)            = []
    | br (NEWTAG _)            = []
    | br (MATRCD (_, _, _))    = []
    | br (MATTAG (_, _, _, l)) = [l]
    | br (MATINT (_, _, l))    = [l]
    | br (CALL   _)            = []

    | br (RAISE   l)           = []

    | br (LABEL  _)            = []
    | br (GOTO b)              = [b]
    | br (RETURN _)            = []
    | br (EXIT )              = []

  fun getLocs (MOV    (l1, l2))       = [l1, l2]
    | getLocs (NEWFCN (l, f))         = [l]
    | getLocs (NEWSCN (l, s))         = [l]
    | getLocs (NEWRCD (l, ls))        = l :: ls
    | getLocs (NEWTAG (l1, l2, c))    = [l1, l2]
    | getLocs (MATRCD (l1, l2, l))    = [l1, l2]
    | getLocs (MATTAG (l1, l2, c, b)) = [l1, l2]
    | getLocs (MATINT (l, i, b))      = [l]
    | getLocs (CALL   (l1, l2, l3))   = [l1, l2, l3]

    | getLocs (RAISE   l)   = [l]

    | getLocs (LABEL  l)              = []
    | getLocs (GOTO l)                = []
    | getLocs (RETURN l)              = [l]
    | getLocs EXIT                    = []


  fun mapLoc l map = Option.getOpt (LMAP.find (map, l), l)

  fun replaceLocs map inst = let
    fun maploc l = Option.getOpt (LMAP.find (map, l), l) in
    case inst of
      (MOV    (l1, l2))       => (MOV    (maploc l1, maploc l2))
    | (NEWFCN (l, f))         => (NEWFCN (maploc l, f))
    | (NEWSCN (l, s))         => (NEWSCN (maploc l, s))
    | (NEWRCD (l, ls))        => (NEWRCD (maploc l, List.map maploc ls))
    | (NEWTAG (l1, l2, c))    => (NEWTAG (maploc l1, maploc l2, c))
    | (MATRCD (l1, l2, l))    => (MATRCD (maploc l1, maploc l2, l))
    | (MATTAG (l1, l2, c, b)) => (MATTAG (maploc l1, maploc l2, c, b))
    | (MATINT (l, i, b))      => (MATINT (maploc l, i, b))
    | (CALL   (l1, l2, l3))   => (CALL   (maploc l1, maploc l2, maploc l3))

    | (RAISE   l)             => (RAISE   (maploc l))

    | (LABEL  l)              => (LABEL  l)
    | (GOTO l)                => (GOTO l)
    | (RETURN l)              => (RETURN (maploc l))
    | EXIT                    => EXIT
  end

  fun toString (MOV    (l1, l2)) = "MOV" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    | toString (NEWSCN (l, sc))  = "NEWSCN"  ^ " " ^ (l2s l) ^ " " ^
    (SC.toString sc)
    | toString (NEWFCN (l, f)) = "NEWFCN" ^ " " ^ (l2s l) ^ " " ^ (i2s f)
    | toString (NEWRCD (l, ls))  = "NEWRCD" ^ " " ^ (l2s l) ^ " " ^
    (ListAux.toString ls l2s " ")
    | toString (NEWTAG (l1, l2, tag)) = "NEWTAG" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s tag)
    | toString (MATRCD (l1, l2, i)) = "MATRCD" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s i)
    | toString (MATTAG (l1, l2, tag, tar)) = "MATTAG" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s tag) ^ " L" ^ (i2s tar)
    | toString (MATINT (l, i, tar)) = "MATINT" ^ " " ^ (l2s l) ^ " " ^
    (i2s i) ^ " L" ^ (i2s tar)
    | toString (CALL   (l1, l2, l3)) = "CALL" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    ^ " " ^ (l2s l3)

    | toString (RAISE l) = "RAISE" ^ " " ^ (l2s l)

    | toString (LABEL  (lab)) = "L" ^ (i2s lab)
    | toString (GOTO i) = "GOTO " ^  (i2s i)
    | toString (RETURN l) = "RETURN" ^ " " ^ (l2s l)
    | toString EXIT = "EXIT"
end
