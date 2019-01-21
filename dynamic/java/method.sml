structure Method = struct

  structure CP = ConstantPool
  structure D = Descriptor
  structure CST = Constant
  structure N = Name

  datatype constant = datatype CP.constant
  datatype typ = datatype D.typ
  datatype desc = datatype D.descriptor
  datatype name = datatype N.name
  datatype utf = datatype CST.utf

  datatype methodacc = datatype MethodAccess.methodacc
  datatype attribute = datatype Attribute.attribute

  type cpindex = int
  type method = methodacc list * cpindex * cpindex * attribute list

  val i2ws2 = WordList.fromInt 2

  fun new (name, desc, codes) cp = let
    val (cp1, id1) = CP.add cp (C_UTF (UTF_NAME (NUNQ name)))
    val (cp2, id2) = CP.add cp1 (C_UTF (UTF_DESC desc)) in
    (([ACC_STATIC], id1, id2, [codes]) : method, cp2) end

  fun add mp m cp = let
    val (method, newcp) = new m cp in
    (mp @ [method], newcp) end

  fun radd mpref m cpref = let
    val (newmp, newcp) = add (! mpref) m (! cpref) in
    mpref := newmp;
    cpref := newcp end

  fun toWords (accflag, nameindex, descindex, attributes) = let
    val af = MethodAccess.toWords accflag
    val ni = i2ws2 nameindex
    val di = i2ws2 descindex
    val at = (List.concat o (List.map Attribute.toWords)) attributes in
    af @ ni @ di @ (i2ws2 (List.length attributes)) @ at end

end
