-- Sort by start, then fold coalescing overlaps (accumulator kept reversed).

def step (acc : List (Int × Int)) (iv : Int × Int) : List (Int × Int) :=
  match acc with
  | []            => [iv]
  | top :: rest   =>
      let (lo, hi) := iv
      if lo <= top.2 then (top.1, max top.2 hi) :: rest
      else (lo, hi) :: top :: rest

def merge (xs : List (Int × Int)) : List (Int × Int) :=
  let sorted := xs.toArray.qsort (fun a b => a.1 < b.1) |>.toList
  (sorted.foldl step []).reverse

#guard merge [(1,3),(2,6),(8,10),(15,18)] == [(1,6),(8,10),(15,18)]
#guard merge [(1,4),(4,5)] == [(1,5)]
