structure Graph = struct

  structure M = Array2
  type graph = bool Array2.array

  fun empty (a, b) = M.array (a, b, false)

  fun falseRow (arr, r) = let
    val cref = ref 0
    val clim = M.nCols arr
    val arrref = ref arr in (
    while (! cref) < clim do
      M.update (!arrref, r, !cref, false));
    ! arrref end

  fun updateByRowTrueList (arr ,r ,cs) = (
    falseRow (arr, r);
    List.map (fn c => M.update (arr, r, c, true)))

  fun addEdge (arr, a, b) = M.update (arr, a, b, true)

  fun succ (arr, v) = M.row    (arr, v)
  fun prev (arr, v) = M.column (arr, v)

end
