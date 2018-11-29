structure Varty = struct

  structure SA = StringAux
  type varty = int

  fun toString n = let
    fun aux 0 = "a"
      | aux n = SA.succ (aux (n - 1))
  in "'" ^ (aux n) end

end
