structure StringKey : ORD_KEY = struct
  type ord_key = string
  val compare = String.compare
end

structure LabKey : ORD_KEY = struct
  datatype ord_key = datatype Identifier.lab

  fun compare (STRLAB s, INTLAB i) = GREATER
    | compare (INTLAB i, STRLAB s) = LESS
    | compare (STRLAB s, STRLAB s') = String.compare (s,s')
    | compare (INTLAB i, INTLAB i') = Int.compare (i,i')

end

structure TynameKey = struct
  type ord_key = Identifier.ltycon * int * bool 
  fun compare ((ltycon, _, _), (ltycon', _, _)) = 
    Identifier.compareLid (ltycon, ltycon')
end
