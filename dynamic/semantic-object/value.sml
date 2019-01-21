structure Value = struct

  structure SV = SpecialValue
  structure BV = BasicValue
  structure A = Address
  structure VID = ValueIdentifier
  structure LM = LabBinaryMap
  structure IM = IntBinaryMapAux
  structure SM = StringBinaryMap
  structure IDS = IdentifierStatus
  structure EN = ExceptionName
  structure SS = StringBinarySet

  type location = int

  datatype space =
    SPA of strspa * tyspa * valspa
  withtype record = location LM.map
  and strspa = space SM.map
  and valspa = (location * IDS.idstat) SM.map
  and tyspa  = valspa SM.map

end
