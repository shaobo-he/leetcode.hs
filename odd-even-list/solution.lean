/-
LeetCode 328: Odd Even Linked List
Group elements at odd positions ahead of those at even positions.
Lean 4, core library only.
-/

def odds : List α → List α
  | x :: _ :: rest => x :: odds rest
  | [x]            => [x]
  | []             => []

def evens : List α → List α
  | _ :: y :: rest => y :: evens rest
  | _              => []

def oddEven (xs : List α) : List α :=
  odds xs ++ evens xs

#guard oddEven [1, 2, 3, 4, 5]     == [1, 3, 5, 2, 4]
#guard oddEven [2, 1, 3, 5, 6, 4, 7] == [2, 3, 6, 7, 1, 5, 4]
