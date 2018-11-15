structure Pos = struct
  type pos = int * int
  fun lineMove (a, b) x = (a + x, b)
  fun letterMove (a, b) x = (a, b + x)
end
