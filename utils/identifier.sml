structure TypeVariable = struct
  type tyvar = string
  fun toString tv = "'" ^ tv
end

structure Identifier = struct
  type id = string
  fun toString x = x
end

structure TypeConstructor = struct
  open Identifier
  type tycon = id
end

structure StructureIdentifier = struct
  open Identifier
  type sid = id
end

structure ValueIdentifier = struct
  open Identifier
  type vid = id
end

structure LongIdentifier = struct
  structure SID = StructureIdentifier
  type lidpre = SID.sid list
  type 'a lid = lidpre * 'a

  fun compare ((p1, i1), (p2, i2)) = let
    val pc = List.collate String.compare (p1, p2) in
    if pc = EQUAL then String.compare (i1, i2) else pc end
  
  fun toString (lidpre, id) = let
    fun aux ids = List.foldr (fn (id ,s) => id ^ "." ^ s) "" ids in
    (aux lidpre) ^ (Identifier.toString id) end
end

structure LongValueIdentifier = struct
  open LongIdentifier
  structure VID = ValueIdentifier
  type lvid = VID.vid lid
end

structure LongTypeConstructor = struct
  open LongIdentifier
  structure TYC = TypeConstructor
  type ltycon = TYC.tycon lid
end

structure LongStructureIdentifier = struct
  open LongIdentifier
  structure SID = StructureIdentifier
  type lvid = SID.sid lid
end

structure IdentifierStatus = struct

  datatype idstat = VAL | CON | EXC

  fun toString VAL = "v"
    | toString CON = "c"
    | toString EXC = "e"

end
