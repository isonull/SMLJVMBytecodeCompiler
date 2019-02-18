structure Descriptor = struct

  datatype name = datatype Name.name

  datatype typ = 
    B | C | D | F | I | J | S | Z | V |
    L of name |
    A of typ

  type mtyp = typ list * typ

  datatype descriptor =
    V_DESC of typ |
    M_DESC of mtyp

  val classTyp = L o NBIN
  val classDesc = V_DESC o classTyp

  fun toString (V_DESC typ) = (case typ of 
      B => "B" 
    | C => "C" 
    | D => "D" 
    | F => "F" 
    | I => "I" 
    | J => "J" 
    | S => "S" 
    | Z => "Z" 
    | V => "V" 
    | (L name) => "L" ^ (Name.toString name) ^ ";"
    | (A typ)  => "[" ^ (toString (V_DESC typ)) )

    | toString (M_DESC (tys, ty)) = let
    val arg = ListAux.toString tys (fn typ => toString (V_DESC typ)) ""
    val ret = toString (V_DESC ty) in
    "(" ^ arg ^ ")" ^ ret end

  val toWords = ModifiedUtf8.fromString o toString

end
