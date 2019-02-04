structure InterInstruction = struct

  structure SC = SpecialConstant

  type loc = int * int
  type fid = int
  datatype scon = datatype SpecialConstant.scon

  datatype code =
    MOV    of loc * loc |

    NEWFCN of loc * fid |
    NEWSCN of loc * scon |
    NEWRCD of loc * ((loc * string) list) |
    NEWCON of loc * loc * int |
    GETRCD of loc * loc * string * label |
    GETCON of loc * loc * int * label |
    GETINT of loc * int * label |

    CALL   of loc * loc * loc |
    IADD   of loc * loc |

    LABEL  of label |
    RETURN of loc

  withtype label = int
  and instruction = code

  val i2s = Int.toString

  fun l2s (a, b) = "(" ^ (i2s a) ^ "," ^ (i2s b) ^ ")"

  fun isBr (MOV    _) = false
    | isBr (NEWFCN _) = false
    | isBr (NEWSCN _) = false
    | isBr (NEWRCD _) = false
    | isBr (NEWCON _) = false
    | isBr (GETRCD _) = true
    | isBr (GETCON _) = true
    | isBr (GETINT _) = true
    | isBr (CALL   _) = false
    | isBr (IADD   _) = false
    | isBr (LABEL  _) = false
    | isBr (RETURN _) = false

  fun br (MOV    _) = NONE
    | br (NEWFCN _) = NONE
    | br (NEWSCN _) = NONE
    | br (NEWRCD _) = NONE
    | br (NEWCON _) = NONE
    | br (GETRCD (_, _, _, l)) = SOME l
    | br (GETCON (_, _, _, l)) = SOME l
    | br (GETINT (_, _, l))    = SOME l
    | br (CALL   _) = NONE
    | br (IADD   _) = NONE
    | br (LABEL  _) = NONE
    | br (RETURN _) = NONE
  

  fun getLocs (MOV    (l1, l2)) = [l1, l2]
    | getLocs (NEWFCN (l, _)) = [l]
    | getLocs (NEWSCN (l, _)) = [l]
    | getLocs (NEWRCD (l, ls)) = l :: (List.map #1 ls)
    | getLocs (NEWCON (l1, l2, _)) = [l1, l2]
    | getLocs (GETRCD (l1, l2, _, _)) = [l1, l2]
    | getLocs (GETCON (l1, l2, _, _)) = [l1, l2]
    | getLocs (GETINT (l, _, _)) = [l]
    | getLocs (CALL   (l1, l2, l3)) = [l1, l2, l3]
    | getLocs (IADD   (l1, l2)) = [l1, l2]
    | getLocs (LABEL  _) = []
    | getLocs (RETURN l) = [l]

  fun toString (MOV    (l1, l2)) = "MOV" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    | toString (NEWSCN (l, sc))  = "NEWSCN"  ^ " " ^ (l2s l) ^ " " ^
    (SC.toString sc)
    | toString (NEWFCN (l, f)) = "NEWFCN" ^ " " ^ (l2s l) ^ " " ^ (i2s f)
    | toString (NEWRCD (l, ls))  = "NEWRCD" ^ " " ^ (l2s l) ^ " " ^
    (ListAux.toString ls (fn (lc, lb) => (l2s lc) ^ " " ^ lb) " ")
    | toString (GETRCD (l1, l2, lab, tar)) = "GETRCD" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ lab ^ " L" ^ (i2s tar)
    | toString (NEWCON (l1, l2, tag)) = "NEWCON" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s tag)
    | toString (GETCON (l1, l2, tag, tar)) = "GETCON" ^ " " ^ (l2s l1) ^ " " ^
    (l2s l2) ^ " " ^ (i2s tag) ^ " L" ^ (i2s tar)
    | toString (CALL   (l1, l2, l3)) = "CALL" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    ^ " " ^ (l2s l3)
    | toString (GETINT (l, i, tar)) = "GETINT" ^ " " ^ (l2s l) ^ " " ^
    (i2s i) ^ " L" ^ (i2s tar)
    | toString (IADD   (l1, l2)) = "IADD" ^ " " ^ (l2s l1) ^ " " ^ (l2s l2)
    | toString (LABEL  (lab)) = "L" ^ (i2s lab)
    | toString (RETURN l) = "RETURN" ^ " " ^ (l2s l)

end
