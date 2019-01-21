structure Name = struct

  datatype name =
    NBIN of string list |
    NUNQ of string |
    NMOD of string list

  fun toString (NBIN ls) = ListAux.toString ls (fn x => x) "/"
    | toString (NUNQ ls) = ls
    | toString (NMOD ls) = ListAux.toString ls (fn x => x) "."

  val toWords = ModifiedUtf8.fromString o toString
 
end
