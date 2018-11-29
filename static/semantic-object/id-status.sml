structure IdStatus = struct

  datatype idstat = VAL | CON | EXC

  fun toString VAL = "v"
    | toString CON = "c"
    | toString EXC = "e"

end
