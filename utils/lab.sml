structure Lab = struct
  datatype lab =
  STR_LAB of string |
  INT_LAB of int

  fun toString (STR_LAB s) = s
    | toString (INT_LAB i) = Int.toString i
end

structure LabKey : ORD_KEY = struct
  datatype ord_key = datatype Lab.lab

  fun compare (STR_LAB s, INT_LAB i) = GREATER
    | compare (INT_LAB i, STR_LAB s) = LESS
    | compare (STR_LAB s, STR_LAB s') = String.compare (s,s')
    | compare (INT_LAB i, INT_LAB i') = Int.compare (i,i')

end

structure LabBinarySet = OrdSetAuxFn (BinarySetFn (LabKey))

structure LabBinaryMap = struct
  exception LabDuplication
  structure LBM = OrdMapAuxFn (BinaryMapFn (LabKey))
  open LBM
  fun fromList lst = let
    (* in SML/NJ this index starts from 1 *)
    val labs = List.tabulate (List.length lst, fn x => Lab.INT_LAB x)
    val listPair = ListPair.zip (labs, lst)
  in fromListPair listPair end

  fun insertUnoccupied map key value = let
    val oriOp = find (map, key) in
    if isSome oriOp then
      raise LabDuplication else
      insert (map, key, value) end
end
