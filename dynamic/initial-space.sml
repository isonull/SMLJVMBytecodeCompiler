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

  val basNameList = [iadd, isub, imul, idiv, ineg, iout]
  val basValueList =
    [VAL (~1, 1), VAL (~1, 2), VAL (~1, 3), VAL (~1, 4), VAL (~1, 0), VAL (~1, 5)]

  val valspa = VS.fromListPair (ListPair.zip (basNameList, basValueList))

  val space = S.fromValspa valspa

end