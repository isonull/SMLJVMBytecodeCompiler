structure InitialSpace = struct 

  structure VS = ValueSpace
  structure S = Space

  datatype value = datatype Value.value
  datatype idstat = datatype IdentifierStatus.idstat

  val iadd = "i+"
  val isub = "i-"
  val imul = "i*"
  val idiv = "i/"
  val ineg = "i~"

  val basList = [iadd, isub, imul, idiv, ineg]
  val basValList = map (fn (s) => (s, (BAS s, VAL))) basList

  val valspa = VS.fromListPair basValList

  val space = S.fromValspa valspa

end
