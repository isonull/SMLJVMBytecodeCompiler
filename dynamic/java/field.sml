structure Field = struct

  structure CP = ConstantPool
  structure D = Descriptor
  structure CST = Constant
  structure N = Name

  datatype constant = datatype CP.constant
  datatype typ = datatype D.typ
  datatype desc = datatype D.descriptor
  datatype name = datatype N.name
  datatype utf = datatype CST.utf

  datatype fieldacc = datatype FieldAccess.fieldacc
  datatype attribute = datatype Attribute.attribute

  type cpindex = int
  type field = fieldacc list * cpindex * cpindex * attribute list

  val i2ws2 = WordList.fromInt 2

  fun new (accs, name, desc) cp = let
    val (cp1, id1) = CP.add cp (C_UTF (UTF_NAME (NUNQ name)))
    val (cp2, id2) = CP.add cp1 (C_UTF (UTF_DESC desc)) in
    (([ACC_STATIC], id1, id2, []) : field, cp2) end

  fun add fp f cp = let
    val (field, newcp) = new f cp in
    (fp @ [field], newcp) end

  fun radd fpref f cpref = let
    val (newfp, newcp) = add (! fpref) f (! cpref) in
    fpref := newfp;
    cpref := newcp end

  fun toWords (accflag, nameindex, descindex, attributes) = let
    val af = FieldAccess.toWords accflag
    val ni = i2ws2 nameindex
    val di = i2ws2 descindex
    val at = (List.concat o (List.map Attribute.toWords)) attributes in
    af @ ni @ di @ (i2ws2 (List.length attributes)) @ at end

end
