structure InterInstruction = struct

  structure SC = SpecialConstant

  type loc = int * int
  type fid = int

  datatype scon = datatype SpecialConstant.scon

  val etagBind  = 0
  val etagMatch = 1

  datatype code =
    MOV    of loc * loc |

    NEWFCN of loc * fid |
    NEWSCN of loc * scon |
    NEWRCD of loc * ((loc * string) list) |
    NEWTAG of loc * loc * int |

    MATRCD of loc * loc * string * label |
    MATTAG of loc * loc * int * label |
    MATINT of loc * int * label |

    CALL   of loc * loc * loc |
    RAISE  of loc |

    IADD   of loc * loc |

    LABEL  of label |
    GOTO   of label |

    EXSTR  of int |
    EXEND  of int |
    HDEND  of int |

    RBIND |
    RMATCH |

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
    | br (MATRCD (_, _, _, l)) = [l]
    | br (MATTAG (_, _, _, l)) = [l]
    | br (MATINT (_, _, l))    = [l]
    | br (CALL   _)            = []
    | br (IADD   _)            = []
    | br (LABEL  _)            = []
    | br (RETURN _)            = []
    | br (GOTO b)              = [b]
    | br RBIND                 = []
    | br RMATCH                = []

  fun getLocs (MOV    (l1, l2))       = [l1, l2]
    | getLocs (NEWFCN (l, f))         = [l]
    | getLocs (NEWSCN (l, s))         = [l]
    | getLocs (NEWRCD (l, ls))        = l :: (List.map #1 ls)
    | getLocs (NEWTAG (l1, l2, c))    = [l1, l2]
    | getLocs (MATRCD (l1, l2, l, b)) = [l1, l2]
    | getLocs (MATTAG (l1, l2, c, b)) = [l1, l2]
    | getLocs (MATINT (l, i, b))      = [l]
    | getLocs (CALL   (l1, l2, l3))   = [l1, l2, l3]
    | getLocs (RAISE   l)   = [l]
    | getLocs (IADD   (l1, l2))       = [l1, l2]
    | getLocs (LABEL  l)              = []
    | getLocs (GOTO l)                = []
    | getLocs RBIND                   = []
    | getLocs RMATCH                  = []
    | getLocs (RETURN l)              = [l]
    | getLocs EXIT                    = []

  fun toString (MOV    (l1, l2)) = "MOV" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    | toString (NEWSCN (l, sc))  = "NEWSCN"  ^ " " ^ (l2s l) ^ " " ^
    (SC.toString sc)
    | toString (NEWFCN (l, f)) = "NEWFCN" ^ " " ^ (l2s l) ^ " " ^ (i2s f)
    | toString (NEWRCD (l, ls))  = "NEWRCD" ^ " " ^ (l2s l) ^ " " ^
    (ListAux.toString ls (fn (lc, lb) => (l2s lc) ^ " " ^ lb) " ")
    | toString (MATRCD (l1, l2, lab, tar)) = "MATRCD" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ lab ^ " L" ^ (i2s tar)
    | toString (NEWTAG (l1, l2, tag)) = "NEWTAG" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s tag)
    | toString (MATTAG (l1, l2, tag, tar)) = "MATTAG" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s tag) ^ " L" ^ (i2s tar)
    | toString (CALL   (l1, l2, l3)) = "CALL" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    ^ " " ^ (l2s l3)
    | toString (RAISE l) = "RAISE" ^ " " ^ (l2s l)
    | toString (MATINT (l, i, tar)) = "MATINT" ^ " " ^ (l2s l) ^ " " ^
    (i2s i) ^ " L" ^ (i2s tar)
    | toString (IADD   (l1, l2)) = "IADD" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    | toString (LABEL  (lab)) = "L" ^ (i2s lab)
    | toString (RETURN l) = "RETURN" ^ " " ^ (l2s l)
    | toString (EXSTR i) = "EXSTR" ^ " " ^ (i2s i)
    | toString (EXEND i) = "EXEND" ^ " " ^ (i2s i)
    | toString (HDEND i) = "HDEND" ^ " " ^ (i2s i)
    | toString RBIND   = "RBIND"
    | toString RMATCH  = "RMATCH"
    | toString (GOTO i) = "GOTO " ^  (i2s i)
    | toString EXIT = "EXIT"
end
