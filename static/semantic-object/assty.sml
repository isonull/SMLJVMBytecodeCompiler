structure Assty = struct

  structure SA = StringAux
  type assty = int

  fun toString n = let
    fun aux 0 = "a"
      | aux n = SA.succ (aux (n - 1))
  in "~" ^ (aux n) end

end

structure AsstySet = struct

  open IntBinarySetAux

  fun getNewAssty set = getExclusion set 0 (fn x => x + 1)
end
