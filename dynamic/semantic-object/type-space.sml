structure TypeSpace = struct

  open StringBinaryMap

  type tyspa = Value.tyspa

  (* notice the vid as a constructor can never be bind to
   * other values because of the pattern inference rule *)

end

