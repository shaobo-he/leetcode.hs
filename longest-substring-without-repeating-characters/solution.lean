-- Longest substring without repeating characters.
-- Sliding window: track the last-seen index per character in an assoc list;
-- when a repeat is seen, jump the window start past its previous position.

-- last-seen index per char as an assoc list
def setIdx (c : Char) (i : Nat) : List (Char × Nat) → List (Char × Nat)
  | []             => [(c, i)]
  | (d, j) :: rest => if c == d then (c, i) :: rest else (d, j) :: setIdx c i rest

def lookupIdx (c : Char) : List (Char × Nat) → Option Nat
  | []             => none
  | (d, j) :: rest => if c == d then some j else lookupIdx c rest

def lengthOfLongest (s : String) : Nat :=
  let rec go (i start : Nat) (seen : List (Char × Nat)) (best : Nat) : List Char → Nat
    | []      => best
    | c :: cs =>
      let start' := match lookupIdx c seen with
                    | some j => max start (j + 1)
                    | none   => start
      go (i + 1) start' (setIdx c i seen) (max best ((i + 1) - start')) cs
  go 0 0 [] 0 s.toList

#guard lengthOfLongest "abcabcbb" == 3
#guard lengthOfLongest "bbbbb" == 1
#guard lengthOfLongest "pwwkew" == 3
