structure InterMethod = struct 
  structure II = InterInstruction

  type labmax = int
  type method = II.instruction list * labmax

  fun toString method = ListAux.toString method II.toString "\n"

end
