structure InterMethod = struct 

  structure II = InterInstruction
  structure ISET = IntBinarySetAux

  datatype code = datatype II.code

  type labmax = int
  type method = II.instruction list
  type block  = (int option) * (int list) * method

  fun genBlocks meth = let

    fun bl (i :: is) b bs l = (case i of
        LABEL m => bl is [] 
          (if List.null b then bs else ((l, [], rev b) :: bs)) (SOME m)
      | _       => (if not (List.null (II.br i)) then
        bl is []       ((l, II.br i, rev (i :: b)) :: bs) NONE else
        bl is (i :: b) bs                        NONE))
      | bl [] [] bs l = bs
      | bl [] b  bs l = (l, [], b) :: bs in

    rev (bl meth [] [] NONE) end

  val getLocs = ListAux.rmDup o
      (List.concat o (List.map (fn i => II.getLocs i))) 

  fun toString method = ListAux.toString method II.toString "\n"

end

structure BasicBlocks = struct

  val i2s = Int.toString

  fun toString (lab, brs, m) = let
    val ls = OptionAux.toString lab i2s "N"
    val br = ListAux.toString brs i2s "," in
    "L" ^ ls ^ "\n BR: " ^ br end
end
