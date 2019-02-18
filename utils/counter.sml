
signature COUNT = sig
  type elem
  val succ : elem -> elem
  val init : elem
end

functor CounterFn (Count : COUNT) = struct
  fun new () = ref Count.init
  fun newi i = ref i
  fun inc ct = ct := (Count.succ (! ct))
  fun next ct = (inc ct; ! ct)
  fun init ct = ct := Count.init
end

structure IntCounter = CounterFn (struct

  type elem = int
  fun succ x = x + 1
  val init = 0

end)
