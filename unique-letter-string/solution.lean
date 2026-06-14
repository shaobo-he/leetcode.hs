-- Count Unique Characters of All Substrings (Lean 4, core library only)

-- Collect the indices (ascending) at which each char occurs.
def appendIdx (c : Char) (i : Int) : List (Char × List Int) → List (Char × List Int)
  | [] => [(c, [i])]
  | (d, is) :: rest =>
    if c == d then (d, is ++ [i]) :: rest else (d, is) :: appendIdx c i rest

def positionsOf (cs : List Char) : List (Char × List Int) :=
  let rec go (i : Int) (acc : List (Char × List Int)) : List Char → List (Char × List Int)
    | [] => acc
    | c :: rest => go (i + 1) (appendIdx c i acc) rest
  go 0 [] cs

-- Each occurrence contributes (i - prev) * (next - i), with prev/next = -1 / n.
def contrib (n : Int) (idxs : List Int) : Int :=
  let ls := (-1) :: idxs
  let rs := idxs.drop 1 ++ [n]
  let triples := ls.zip (idxs.zip rs)
  (triples.map (fun (l, i, r) => (i - l) * (r - i))).foldl (· + ·) 0

def uniqueLetterString (s : String) : Int :=
  let cs := s.toList
  let n : Int := Int.ofNat cs.length
  ((positionsOf cs).map (fun p => contrib n p.snd)).foldl (· + ·) 0

#guard uniqueLetterString "ABC" == 10
#guard uniqueLetterString "ABA" == 8
#guard uniqueLetterString "LEETCODE" == 92
