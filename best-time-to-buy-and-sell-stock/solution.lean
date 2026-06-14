-- Single pass: lowest price so far + best profit.
def maxProfit : List Int → Int
  | []      => 0
  | p :: ps =>
    let rec go (lowest best : Int) : List Int → Int
      | []      => best
      | x :: xs => go (min lowest x) (max best (x - lowest)) xs
    go p 0 ps

#guard maxProfit [7,1,5,3,6,4] == 5
#guard maxProfit [7,6,4,3,1]   == 0
