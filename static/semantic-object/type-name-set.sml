structure TypeNameSet = struct
  open TypeNameBinarySet

  type typenameset = set
  fun prefixSid set sid = map (fn ((p,v), a, e) => ((sid :: p, v), a, e)) set
end
