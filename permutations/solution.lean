-- Permutations (Lean 4, core library only)

-- Each element picked as head, paired with the remaining list.
def selections {α : Type} : List α → List (α × List α)
  | [] => []
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

-- Total: each recursive call is on a remainder strictly shorter than the input.
def permute {α : Type} : List α → List (List α)
  | [] => [[]]
  | x :: xs =>
    -- `.attach` carries each element's membership proof, so termination can use it
    (selections (x :: xs)).attach.flatMap
      (fun s => (permute s.val.2).map (fun q => s.val.1 :: q))
  termination_by l => l.length
  decreasing_by
    simp_wf
    exact selections_length _ _ _ s.property

-- 3 distinct elements -> 3! = 6 permutations, each of length 3.
#guard (permute [1, 2, 3]).length == 6
#guard (permute [1, 2, 3]).all (fun p => p.length == 3)
-- Exact enumeration order matches the .hs/.idr "pick head, recurse" scheme.
#guard permute [1, 2, 3] ==
  [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
