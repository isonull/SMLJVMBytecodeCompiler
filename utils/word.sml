structure WordAux = struct

  val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord

end
