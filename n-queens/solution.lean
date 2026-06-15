-- N-Queens: count distinct solutions.
-- A placement assigns one column per successive row; we enumerate permutations
-- of [1..n] (columns are then automatically distinct) and keep the diagonal-safe
-- ones, counting how many survive.

-- All ways to pick one element out of a list, paired with the remaining list.
def selections : List α → List (α × List α)
  | []      => []
  | x :: xs => (x, xs) :: (selections xs).map (fun (y, ys) => (y, x :: ys))

-- every selection pairs a head with a strictly shorter remainder
theorem selections_length {α} : (xs : List α) → (a : α) → (rest : List α) →
    (a, rest) ∈ selections xs → rest.length < xs.length
  | [], _, _, h => by simp [selections] at h
  | x :: xs, a, rest, h => by
    simp only [selections, List.mem_cons, List.mem_map, Prod.mk.injEq] at h
    rcases h with ⟨_, hrx⟩ | ⟨⟨y, ys⟩, hp, _, hpr⟩
    · subst hrx; simp only [List.length_cons]; omega
    · subst hpr
      have := selections_length xs y ys hp
      simp only [List.length_cons]; omega

-- All permutations of a list.  Total: each recursive call is on a strictly
-- shorter remainder (`.attach` carries the membership proof to termination).
def permute : List α → List (List α)
  | []  => [[]]
  | x :: xs => (selections (x :: xs)).attach.flatMap
      (fun s => (permute s.val.2).map (s.val.1 :: ·))
  termination_by l => l.length
  decreasing_by
    simp_wf
    exact selections_length _ _ _ s.property

-- Two queens at (r1,c1) and (r2,c2) don't attack diagonally.
def ok (p q : Int × Int) : Bool :=
  (p.1 - q.1).natAbs ≠ (p.2 - q.2).natAbs

-- cols is the column chosen per row; pair with row indices 1..len and check
-- every pair of placements is diagonal-safe.
def diagSafe (cols : List Int) : Bool :=
  let rows : List Int := (List.range cols.length).map (fun i => Int.ofNat (i + 1))
  let placed := rows.zip cols
  let rec check : List (Int × Int) → Bool
    | []      => true
    | p :: ps => (ps.all (ok p)) && check ps
  check placed

def queens (n : Nat) : List (List Int) :=
  let cols : List Int := (List.range n).map (fun i => Int.ofNat (i + 1))
  (permute cols).filter diagSafe

#guard (queens 4).length == 2
#guard (queens 5).length == 10
#guard (queens 6).length == 4
