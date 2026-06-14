-- Permutations II (unique permutations of a multiset, Lean 4, core library only)

-- Multiset as an assoc list of (value, count).
def bump {α : Type} [BEq α] (x : α) : List (α × Nat) → List (α × Nat)
  | [] => [(x, 1)]
  | (y, n) :: rest => if x == y then (y, n + 1) :: rest else (y, n) :: bump x rest

def countsOf {α : Type} [BEq α] (xs : List α) : List (α × Nat) :=
  xs.foldl (fun acc x => bump x acc) []

def decrCount {α : Type} [BEq α] (x : α) : List (α × Nat) → List (α × Nat) :=
  List.map (fun (y, n) => if y == x then (y, n - 1) else (y, n))

partial def permsFromCounts {α : Type} [BEq α] (cs : List (α × Nat)) : List (List α) :=
  if cs.all (fun p => p.snd == 0) then
    [[]]
  else
    cs.flatMap (fun p =>
      if p.snd == 0 then []
      else (permsFromCounts (decrCount p.fst cs)).map (fun rest => p.fst :: rest))

def permuteUnique {α : Type} [BEq α] (xs : List α) : List (List α) :=
  permsFromCounts (countsOf xs)

-- [1,1,2] has 3 unique permutations.
#guard (permuteUnique [1, 1, 2]).length == 3
#guard permuteUnique [1, 1, 2] == [[1, 1, 2], [1, 2, 1], [2, 1, 1]]
