structure Space = struct

  structure SS = StructureSpace
  structure TS = TypeSpace
  structure VS = ValueSpace

  structure SM = StringBinaryMap
  structure VIS = ValueIndexSet

  datatype space = datatype Value.space

  val empty = SPA (SS.empty, TS.empty, VS.empty)

  fun getValstr (SPA (se, _, ve)) (sid :: pre, vid) = let
    val envOp = SS.find (se, sid) in
    Option.join (Option.map (fn e => getValstr e (pre, vid)) envOp) end
    | getValstr (SPA (_, _, ve)) ([], vid) = VS.find (ve, vid)

  fun getTystr (SPA (se, te, _)) (sid :: pre, tycon) = let
    val envOp = SS.find (se, sid) in
    Option.join (Option.map (fn e => getTystr e (pre, tycon)) envOp) end
    | getTystr (SPA (_, te, _)) ([], tycon) = TS.find (te, tycon)

  fun fromValspa vs = SPA (SS.empty, TS.empty, vs)

  fun fromTyspa  ts = SPA (SS.empty, ts, VS.empty)

  fun fromStrspa ss = SPA (ss, TS.empty, VS.empty)

  (* separate the location duplication *)
  fun modify (SPA (se, te, ve)) (SPA (se', te', ve')) = SPA (
    SS.unionWith #2 (se, se'),
    TS.unionWith #2 (te, te'),
    VS.unionWith #2 (ve, ve'))

  fun modifyValspa s vs = modify s (fromValspa vs)
  fun modifyTyspa s ts = modify s (fromTyspa ts)


  fun getIdset (SPA (ss, ts, vs)) = VS.getIdset vs

  fun newId spa = let
    val idset = getIdset spa in
    VIS.new idset end

end
