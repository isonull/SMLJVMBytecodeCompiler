structure InitialSpace = struct 

  structure VS = ValueSpace
  structure S = Space

  datatype value = datatype Value.value

  val iadd = "iadd"
  val isub = "isub"
  val imul = "imul"
  val idiv = "idiv"
  val ineg = "ineg"
  val iout = "iout"
  val tr   = "true"
  val fl   = "false"
  val match= "Match"
  val bind = "Bind"

  val matchLoc = (~1, 8)
  val bindLoc = (~1, 9)

  val basNameList = [iadd, isub, imul, idiv, ineg, iout, tr, fl, match, bind]
  val basValueList =
    [VAL (~1, 1), VAL (~1, 2), VAL (~1, 3), VAL (~1, 4), VAL (~1, 0), 
    VAL (~1, 5), CON((~1, 6), 0), CON((~1, 7), 1), EXC((~1, 8), 0), EXC((~1, 9), 1)]

  val valspa = VS.fromListPair (ListPair.zip (basNameList, basValueList))

  val space = S.fromValspa valspa

end
