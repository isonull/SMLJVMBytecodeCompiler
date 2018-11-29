structure Lab = struct
  datatype lab =
  STRLAB of string |
  INTLAB of int

  fun toString (STRLAB s) = s
    | toString (INTLAB i) = Int.toString i
end

structure LabKey : ORD_KEY = struct
  datatype ord_key = datatype Lab.lab

  fun compare (STRLAB s, INTLAB i) = GREATER
    | compare (INTLAB i, STRLAB s) = LESS
    | compare (STRLAB s, STRLAB s') = String.compare (s,s')
    | compare (INTLAB i, INTLAB i') = Int.compare (i,i')

end

structure LabBinarySet = OrdSetAuxFn (BinarySetFn (LabKey))

structure LabBinaryMap = struct
  structure LBM = OrdMapAuxFn (BinaryMapFn (LabKey))
  open LBM
  fun fromList lst = let
    val labs = List.tabulate (List.length lst, fn x => Lab.INTLAB x)
    val listPair = ListPair.zip (labs, lst)
  in fromListPair listPair end
end
