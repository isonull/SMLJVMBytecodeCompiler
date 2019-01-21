structure Class = struct

  datatype constant = datatype Constant.constant
  datatype classacc = datatype ClassAccess.classacc
  datatype attribute = datatype Attribute.attribute
  type method = Method.method
  type field = Field.field

  type cpindex = int
  type version = int

  type class = (constant list) ref * classacc list *
    cpindex * cpindex * cpindex list * 
    (field list) ref * (method list) ref * (attribute list) ref

  val i2ws2 = WordList.fromInt 2

  val magic = [0wxCA, 0wxFE, 0wxBA, 0wxBE]
  val minorVersion = i2ws2 0
  val majorVersion = i2ws2 55


  fun listToWords f ls = (List.concat o (List.map f)) ls
  fun countWords ls = i2ws2 (List.length ls)

  fun toWords (cp, accflag, this, super, interfaces,
               fields, methods, attributes) = let
    val cp = ! cp
    val fields = ! fields
    val methods = ! methods
    val attributes = ! attributes in
    magic @
    minorVersion @
    majorVersion @
    i2ws2 ((List.length cp) + 1) @
    listToWords Constant.toWords cp @
    ClassAccess.toWords accflag @
    i2ws2 this @
    i2ws2 super @
    countWords interfaces @
    listToWords i2ws2 interfaces @
    countWords fields @
    listToWords Field.toWords fields @
    countWords methods @
    listToWords Method.toWords methods @
    countWords attributes @
    listToWords Attribute.toWords attributes end

  fun write c f = let
    val bs = Word8Vector.fromList (List.map WordAux.wordToWord8 (toWords c))
    val os = BinIO.openOut f in
    BinIO.output (os, bs);
    BinIO.closeOut os end

end
