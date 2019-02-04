structure ListAux = struct
  exception EmptyList

  fun max [x] = x
    | max [] = raise EmptyList
    | max (x :: xs) = (fn (a, b) => if a > b then a else b) (x, max xs)

  fun sub (x :: xs) (s, s') =
    if x = s then s' :: sub xs (s, s') else x :: sub xs (s, s')
    | sub [] (s, s') = []

  fun subsl xs subs = foldl (fn (sb, xs) => sub xs sb) xs subs
  fun subsr xs subs = foldr (fn (sb, xs) => sub xs sb) xs subs

  fun toString nil tostr splitStr = ""
    | toString ls tostr splitStr =
    List.foldr (fn (a, b) => (a ^ splitStr ^ b)) (tostr (List.last ls))
               (map tostr (List.take (ls, (length ls) - 1)))

  fun prependOption (ls, l) = case l of NONE => ls | SOME i => i :: ls
  fun fromOptionList (l :: ls) = (case l of 
      NONE => fromOptionList ls 
    | SOME i => i :: (fromOptionList ls))
    | fromOptionList [] = []

  fun enumerate 0 _ _ = []
    | enumerate n init succ =
    if n < 0 then raise Size else
      init :: (enumerate (n - 1) (succ init) succ)

  fun findIndexFrom (x :: xs) y c = 
    if x = y then SOME c else findIndexFrom xs y (c + 1)
    | findIndexFrom [] _ _ = NONE

  fun findIndex xs y = findIndexFrom xs y 0

end

structure ListPairAux = struct
  fun append (ls1, ls2) (ls1', ls2') = (ls1 @ ls1', ls2 @ ls2')
end
