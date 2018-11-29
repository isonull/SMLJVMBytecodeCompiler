structure Assty = struct

  structure SA = StringAux
  type assty = int

  fun toString n = let
    fun aux 0 = "a"
      | aux n = SA.succ (aux (n - 1))
  in "~" ^ (aux n) end

end
