structure ValueStructure = struct

  structure TS = TypeScheme
  structure IS = IdStatus

  type valstr = TS.tysch * IS.idstat

  fun instantiate (ts, s) is = (TS.instantiate ts is, s)

  fun toString (ts, is) =
    (TS.toString ts) ^ "," ^ (IS.toString is)
end
