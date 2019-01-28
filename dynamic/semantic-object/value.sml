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

  datatype value =
    LOC of int |
    GLB of int |
    FLD of string |
    FCN of string |
    BAS of string |
    STK of int
  
  and space =
    SPA of strspa * tyspa * valspa

  withtype record = location LM.map
  and strspa = space SM.map
  and valspa = (value * IDS.idstat) SM.map
  and tyspa  = valspa SM.map

end
