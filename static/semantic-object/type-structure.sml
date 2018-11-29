structure TypeStructure = struct

  structure VE = ValueEnvironment
  structure TF = TypeFunction

  type tystr = TF.tyfcn * VE.valenv

  fun toString (tf, ve) =
    (TF.toString tf) ^ ",\n" ^ (VE.toString ve)

end
