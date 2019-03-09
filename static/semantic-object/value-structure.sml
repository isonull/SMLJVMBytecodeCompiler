structure ValueStructure = struct

  structure TS = TypeScheme
  structure IS = IdentifierStatus

  type valstr = TS.tysch * IS.idstat

  fun instantiate (ts, s) is = (TS.instantiate ts is, s)

  fun getAsstyset (ts, s) = TS.getAsstyset ts

  fun noWildRowty (ts, s) = TS.noWildRowty ts

  fun toString (ts, is) =
    (TS.toString ts) ^ "," ^ (IS.toString is)

end
