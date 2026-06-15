-- Permutations II (unique permutations of a multiset, Lean 4, core library only)

-- Multiset as an assoc list of (value, count).
def bump {α : Type} [BEq α] (x : α) : List (α × Nat) → List (α × Nat)
  | [] => [(x, 1)]
  | (y, n) :: rest => if x == y then (y, n + 1) :: rest else (y, n) :: bump x rest

def countsOf {α : Type} [BEq α] (xs : List α) : List (α × Nat) :=
  xs.foldl (fun acc x => bump x acc) []

def decrCount {α : Type} [BEq α] (x : α) : List (α × Nat) → List (α × Nat) :=
  List.map (fun (y, n) => if y == x then (y, n - 1) else (y, n))

-- total remaining count, as an explicit (simp_wf-opaque) measure
def totalCount {α} : List (α × Nat) → Nat
  | []             => 0
  | (_, n) :: rest => n + totalCount rest

-- decrementing never grows the total …
theorem totalCount_le {α} [BEq α] (x : α) (cs : List (α × Nat)) :
    totalCount (decrCount x cs) ≤ totalCount cs := by
  induction cs with
  | nil => simp [decrCount, totalCount]
  | cons p rest ih =>
    obtain ⟨y, m⟩ := p
    simp only [decrCount, List.map_cons] at ih ⊢
    split <;> simp only [totalCount] <;> omega

-- … and strictly shrinks it when the decremented key is present with count > 0
theorem totalCount_lt {α} [BEq α] [LawfulBEq α] (x : α) (n : Nat) :
    (cs : List (α × Nat)) → (x, n) ∈ cs → n ≠ 0 →
    totalCount (decrCount x cs) < totalCount cs
  | [], hmem, _ => by simp at hmem
  | (y, m) :: rest, hmem, hn => by
    simp only [decrCount, List.map_cons]
    rcases List.mem_cons.1 hmem with heq | hrest
    · injection heq with hxy hnm
      subst hxy; subst hnm
      have hle := totalCount_le x rest
      simp only [decrCount] at hle
      simp only [beq_self_eq_true, if_true, totalCount]
      omega
    · have hlt := totalCount_lt x n rest hrest hn
      simp only [decrCount] at hlt
      split <;> simp only [totalCount] <;> omega

-- Total: each recursive call decrements a present, positive count, so the total
-- count strictly drops (`.attach` carries the membership proof to termination).
def permsFromCounts {α : Type} [BEq α] [LawfulBEq α] (cs : List (α × Nat)) : List (List α) :=
  if cs.all (fun p => p.snd == 0) then
    [[]]
  else
    cs.attach.flatMap (fun s =>
      if s.val.snd == 0 then []
      else (permsFromCounts (decrCount s.val.fst cs)).map (fun rest => s.val.fst :: rest))
  termination_by totalCount cs
  decreasing_by
    simp_wf
    rename_i h
    exact totalCount_lt s.val.fst s.val.snd cs s.property (by simpa using h)

def permuteUnique {α : Type} [BEq α] [LawfulBEq α] (xs : List α) : List (List α) :=
  permsFromCounts (countsOf xs)

-- [1,1,2] has 3 unique permutations.
#guard (permuteUnique [1, 1, 2]).length == 3
#guard permuteUnique [1, 1, 2] == [[1, 1, 2], [1, 2, 1], [2, 1, 1]]
