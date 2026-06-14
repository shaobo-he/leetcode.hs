-- Kadane's algorithm.
def maxSubArray : List Int → Int
  | []      => 0
  | x :: xs =>
    let step : (Int × Int) → Int → (Int × Int) :=
      fun (cur, best) n => let cur' := max n (cur + n); (cur', max best cur')
    (xs.foldl step (x, x)).snd

#guard maxSubArray [-2,1,-3,4,-1,2,1,-5,4] == 6
#guard maxSubArray [5,4,-1,7,8]            == 23
