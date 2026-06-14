/-
LeetCode 340: Longest Substring with At Most K Distinct Characters
Sliding window holding at most k distinct chars, with per-char counts.
Lean 4, core library only.
-/

def maxN (a b : Nat) : Nat := if a < b then b else a

def incr (x : Char) : List (Char × Nat) → List (Char × Nat)
  | [] => [(x, 1)]
  | (y, n) :: rest =>
      if x == y then (y, n + 1) :: rest else (y, n) :: incr x rest

def decr (x : Char) : List (Char × Nat) → List (Char × Nat)
  | [] => []
  | (y, n) :: rest =>
      if x == y then
        (match n with
         | k + 2 => (y, k + 1) :: rest
         | _     => rest)
      else (y, n) :: decr x rest

partial def shrinkW (k : Nat) (win : List Char) (counts : List (Char × Nat)) :
    List Char × List (Char × Nat) :=
  if counts.length > k then
    match win with
    | d :: ds => shrinkW k ds (decr d counts)
    | []      => (win, counts)
  else (win, counts)

partial def goW (k : Nat) (win : List Char) (counts : List (Char × Nat))
    (best : Nat) : List Char → Nat
  | []      => best
  | c :: cs =>
      let (win2, counts2) := shrinkW k (win ++ [c]) (incr c counts)
      goW k win2 counts2 (maxN best win2.length) cs

def lenKDistinct (s : String) (k : Nat) : Nat :=
  goW k [] [] 0 s.toList

#guard lenKDistinct "eceba" 2 == 3
#guard lenKDistinct "aa" 1    == 2
#guard lenKDistinct "abee" 1  == 2
