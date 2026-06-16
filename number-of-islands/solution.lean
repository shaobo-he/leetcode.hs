/-
LeetCode 200: Number of Islands
Flood-fill DFS over a grid of '1' (land) / '0' (water).
Grid is a List String (rows). Out-of-bounds reads as water.
Lean 4, core library only.
-/

abbrev Grid := List (List Char)

def nth : Nat → List α → Option α
  | _,     []      => none
  | 0,     x :: _  => some x
  | n + 1, _ :: xs => nth n xs

-- grid lookup with Int coords; out of bounds reads as water
def cellAt (grid : Grid) (r c : Int) : Char :=
  if r < 0 || c < 0 then '0'
  else match nth r.toNat grid with
       | none     => '0'
       | some row => (nth c.toNat row).getD '0'

def posEq (a b : Int × Int) : Bool := a.1 == b.1 && a.2 == b.2

def memPos (p : Int × Int) (xs : List (Int × Int)) : Bool :=
  xs.any (posEq p)

-- nth in bounds
theorem nth_lt : (k : Nat) → (l : List α) → (x : α) → nth k l = some x → k < l.length
  | _,     [],      _, h => by simp [nth] at h
  | 0,     _ :: _,  _, _ => Nat.succ_pos _
  | k + 1, _ :: xs, x, h => by
      simp only [nth] at h; have := nth_lt k xs x h; simp only [List.length_cons]; omega

-- the finite universe of in-bounds cell positions (one per actual cell)
def cellsFrom (base : Nat) : Grid → List (Int × Int)
  | []          => []
  | row :: rows =>
      ((List.range row.length).map (fun (c : Nat) => ((base : Int), (c : Int)))) ++ cellsFrom (base + 1) rows

def cells (grid : Grid) : List (Int × Int) := cellsFrom 0 grid

theorem mem_cellsFrom : (grid : Grid) → (base k : Nat) → (row : List Char) → (c : Nat) →
    nth k grid = some row → c < row.length →
    (((base + k : Nat) : Int), ((c : Nat) : Int)) ∈ cellsFrom base grid
  | [],      _,    _,     _,   _, hnth, _  => by simp [nth] at hnth
  | _ :: _,  base, 0,     row, c, hnth, hc => by
      simp only [nth, Option.some.injEq] at hnth; subst hnth
      simp only [cellsFrom, Nat.add_zero, List.mem_append, List.mem_map, List.mem_range]
      exact Or.inl ⟨c, hc, rfl⟩
  | _ :: gs, base, k + 1, row, c, hnth, hc => by
      simp only [nth] at hnth
      have ih := mem_cellsFrom gs (base + 1) k row c hnth hc
      have he : base + (k + 1) = base + 1 + k := by omega
      simp only [cellsFrom, List.mem_append, he]; exact Or.inr ih

-- a land cell is in the universe
theorem mem_cells {grid : Grid} {r c : Int} (h : cellAt grid r c = '1') :
    (r, c) ∈ cells grid := by
  unfold cellAt at h
  split at h
  · exact absurd h (by decide)
  · rename_i hb
    simp only [Bool.not_eq_true, Bool.or_eq_false_iff, decide_eq_false_iff_not] at hb
    obtain ⟨hr0', hc0'⟩ := hb
    have hr0 : 0 ≤ r := by omega
    have hc0 : 0 ≤ c := by omega
    split at h
    · exact absurd h (by decide)
    · rename_i row hrow
      have hc1 : nth c.toNat row = some '1' := by
        cases hcc : nth c.toNat row with
        | none   => rw [hcc] at h; exact absurd h (by decide)
        | some x => rw [hcc] at h; simp only [Option.getD_some] at h; rw [h]
      have hclt : c.toNat < row.length := nth_lt _ _ _ hc1
      have key := mem_cellsFrom grid 0 r.toNat row c.toNat hrow hclt
      simp only [Nat.zero_add, Int.toNat_of_nonneg hr0, Int.toNat_of_nonneg hc0] at key
      exact key

-- `memPos` reflects List membership (Int × Int has lawful `==`)
theorem memPos_iff (p : Int × Int) (xs : List (Int × Int)) : memPos p xs = true ↔ p ∈ xs := by
  simp only [memPos, List.any_eq_true, posEq, Bool.and_eq_true, beq_iff_eq]
  constructor
  · rintro ⟨x, hx, h1, h2⟩; rw [show p = x from Prod.ext h1 h2]; exact hx
  · intro hp; exact ⟨p, hp, rfl, rfl⟩

-- measure: number of cells not yet visited
def remaining (grid : Grid) (visited : List (Int × Int)) : Nat :=
  (cells grid).countP (fun p => decide (p ∉ visited))

theorem countP_le {α} (l : List α) (p q : α → Bool) (himp : ∀ x, q x → p x) :
    l.countP q ≤ l.countP p := by
  induction l with
  | nil => simp
  | cons x xs ih =>
    rw [List.countP_cons, List.countP_cons]
    have : (if q x then 1 else 0) ≤ (if p x then 1 else 0 : Nat) := by
      by_cases hqx : q x <;> simp_all
    omega

theorem countP_lt {α} (l : List α) (p q : α → Bool) (himp : ∀ x, q x → p x)
    (a : α) (ha : a ∈ l) (hqa : q a = false) (hpa : p a = true) :
    l.countP q < l.countP p := by
  induction l with
  | nil => simp at ha
  | cons x xs ih =>
    rw [List.countP_cons, List.countP_cons]
    rcases List.mem_cons.1 ha with rfl | hxs
    · have hle := countP_le xs p q himp
      have hq0 : (if q a then 1 else 0 : Nat) = 0 := by simp [hqa]
      have hp1 : (if p a then 1 else 0 : Nat) = 1 := by simp [hpa]
      omega
    · have ihlt := ih hxs
      have hqp : (if q x then 1 else 0 : Nat) ≤ (if p x then 1 else 0) := by
        by_cases hqx : q x <;> simp_all
      omega

theorem remaining_mono (grid : Grid) {v1 v2 : List (Int × Int)} (hsub : v1 ⊆ v2) :
    remaining grid v2 ≤ remaining grid v1 := by
  apply countP_le
  intro x hx
  simp only [decide_eq_true_eq] at hx ⊢
  exact fun hv => hx (hsub hv)

theorem remaining_lt (grid : Grid) (pos : Int × Int) (visited : List (Int × Int))
    (hmem : pos ∈ cells grid) (hvis : pos ∉ visited) :
    remaining grid (pos :: visited) < remaining grid visited := by
  refine countP_lt (cells grid) (fun p => decide (p ∉ visited))
    (fun p => decide (p ∉ pos :: visited)) ?_ pos hmem ?_ ?_
  · intro x hx
    simp only [decide_eq_true_eq] at hx ⊢
    exact fun hv => hx (List.mem_cons_of_mem _ hv)
  · simp
  · simp only [decide_eq_true_eq]; exact hvis

-- flood fill: add every connected land cell.  Total (verified DFS): `flood`
-- returns a proof `visited ⊆ result`, and the recursion shrinks `remaining`
-- (unvisited cells) — the first neighbour adds the fresh `pos`, the later three
-- only grow `visited`, so monotonicity (carried in the return type) keeps them
-- below the original count.
def flood (grid : Grid) (pos : Int × Int) (visited : List (Int × Int)) :
    { v' : List (Int × Int) // visited ⊆ v' } :=
  if h : cellAt grid pos.1 pos.2 != '1' || memPos pos visited then
    ⟨visited, fun _ hx => hx⟩
  else
    let r1 := flood grid (pos.1 + 1, pos.2) (pos :: visited)
    let r2 := flood grid (pos.1 - 1, pos.2) r1.val
    let r3 := flood grid (pos.1, pos.2 + 1) r2.val
    let r4 := flood grid (pos.1, pos.2 - 1) r3.val
    ⟨r4.val, fun x hx =>
      r4.property (r3.property (r2.property (r1.property (List.mem_cons_of_mem _ hx))))⟩
  termination_by remaining grid visited
  decreasing_by
    all_goals simp_wf
    all_goals (
      simp only [Bool.not_eq_true, Bool.or_eq_false_iff] at h
      obtain ⟨hne, hmp⟩ := h
      have hcell : cellAt grid pos.1 pos.2 = '1' := by
        rw [← beq_iff_eq]; simpa using hne
      have hpc : pos ∈ cells grid := mem_cells hcell
      have hpv : pos ∉ visited := by
        intro hh; rw [← memPos_iff] at hh; rw [hmp] at hh; exact absurd hh (by decide)
      have hbase := remaining_lt grid pos visited hpc hpv
      first
        | exact hbase
        | exact Nat.lt_of_le_of_lt (remaining_mono grid r1.property) hbase
        | exact Nat.lt_of_le_of_lt
            (remaining_mono grid (fun x hx => r2.property (r1.property hx))) hbase
        | exact Nat.lt_of_le_of_lt
            (remaining_mono grid (fun x hx => r3.property (r2.property (r1.property hx)))) hbase)

def numIslands (grid : Grid) : Nat :=
  let rows : Int := grid.length
  let cols : Int := (grid.head?.map List.length).getD 0
  let coords : List (Int × Int) :=
    (List.range rows.toNat).flatMap (fun r =>
      (List.range cols.toNat).map (fun c => (((r : Nat) : Int), ((c : Nat) : Int))))
  let step : (List (Int × Int) × Nat) → (Int × Int) → (List (Int × Int) × Nat) :=
    fun (visited, count) pos =>
      if cellAt grid pos.1 pos.2 == '1' && !memPos pos visited then
        ((flood grid pos visited).val, count + 1)
      else
        (visited, count)
  (coords.foldl step ([], 0)).2

def mkGrid (rows : List String) : Grid := rows.map String.toList

#guard numIslands (mkGrid ["11110", "11010", "11000", "00000"]) == 1
#guard numIslands (mkGrid ["11000", "11000", "00100", "00011"]) == 3
