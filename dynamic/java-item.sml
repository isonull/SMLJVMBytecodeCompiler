structure JavaItem = struct

  structure D = Descriptor
  structure CST = Constant
  structure N = Name
  structure CP = ConstantPool
  structure F = Field
  structure M = Method

  datatype constant = datatype CP.constant
  datatype typ = datatype D.typ
  datatype desc = datatype D.descriptor
  datatype name = datatype N.name
  datatype utf = datatype CST.utf
  datatype instruction = datatype Instruction.instruction

  val objectName = ["java","lang","Object"]
  val iCname = ["java","lang","Integer"]
  val fCname = ["java","lang","Float"]
  val conCname = ["Con"]
  val mapCname = ["java", "util", "HashMap"]
  val methodHandleName = ["java","lang","invoke","MethodHandle"]

  val envDesc = V_DESC (A (L (NBIN objectName)))
  val initDesc = M_DESC ([], V)
  val fenvDesc = V_DESC(A (L (NBIN methodHandleName)))
  val putDesc = M_DESC ([(L o NBIN) objectName, (L o NBIN) objectName], 
  (L o NBIN) objectName)
  val conPutDesc = M_DESC ([(L o NBIN) iCname, (L o NBIN) objectName], V)

  val methodDesc = M_DESC ([(L o NBIN) objectName], (L o NBIN) objectName)
  val ivalofDesc = M_DESC ([I], (L o NBIN) iCname)

  val conGetDesc = M_DESC ([(L o NBIN) iCname], (L o NBIN) objectName)

  fun newClass name = let
    val cpref = ref ([] : CST.constant list)
    val fpref = ref ([] : Field.field list)
    val mpref = ref ([] : Method.method list)

    val cidThis    = CP.radd cpref (C_CLASS name)
    val cidSuper   = CP.radd cpref (C_CLASS objectName)

  in
    (cpref,
    [Class.ACC_PUBLIC],
    cidThis,
    cidSuper,
    [],
    fpref,
    mpref,
    ref []
    ) : Class.class end

end
