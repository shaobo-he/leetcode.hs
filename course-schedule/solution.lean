-- Topological sort via DFS, with status map (1 = on stack, 2 = finished);
-- `none` signals a cycle.  Lean 4, core library only.
--
-- TOTAL (verified termination).  The DFS explores arbitrary graph successors,
-- not structure, so termination is a finite-measure argument:
--   * `graphNodes`/`univ` is the finite universe of nodes that can ever be
--     visited (graph keys+values, plus the initial 0..n-1).
--   * `unvisited univ st` = number of universe nodes with no entry in the status
--     map `st`.  Visiting a fresh node adds it to `st`, dropping the count.
--   * the recursion runs through a successor loop, so the real measure is the
--     lexicographic pair `(unvisited univ st, rank)` — `visit` ranks below the
--     successor loop, and the loop falls back to the remaining-list length.
--   * monotonicity of the status map (it only grows) is carried in the return
--     type, so the loop's continuation provably does not raise `unvisited`.
-- No fuel, no `partial`.

def addEdge (k v : Int) : List (Int × List Int) → List (Int × List Int)
  | [] => [(k, [v])]
  | (k', vs) :: rest =>
    if k == k' then (k', v :: vs) :: rest else (k', vs) :: addEdge k v rest

def buildGraph (prereqs : List (List Int)) : List (Int × List Int) :=
  prereqs.foldl (fun g e =>
    match e with
    | a :: b :: _ => addEdge b a g
    | _           => g) []

def succsOf (node : Int) (g : List (Int × List Int)) : List Int :=
  match g.find? (fun p => p.fst == node) with
  | some (_, vs) => vs
  | none         => []

-- the finite universe of graph nodes (every key and every value)
def graphNodes (g : List (Int × List Int)) : List Int :=
  g.flatMap (fun p => p.fst :: p.snd)

theorem succ_mem_graphNodes (g : List (Int × List Int)) (node s : Int)
    (hs : s ∈ succsOf node g) : s ∈ graphNodes g := by
  unfold succsOf at hs
  cases hf : g.find? (fun p => p.fst == node) with
  | none => rw [hf] at hs; simp at hs
  | some kv =>
    rw [hf] at hs
    obtain ⟨k, vs⟩ := kv
    simp only at hs
    have hmem : (k, vs) ∈ g := List.mem_of_find?_eq_some hf
    unfold graphNodes
    rw [List.mem_flatMap]
    exact ⟨(k, vs), hmem, List.mem_cons_of_mem _ hs⟩

-- a node is "covered" when it has any entry in the status map
def covered (st : List (Int × Int)) (nd : Int) : Bool :=
  (st.find? (fun p => p.fst == nd)).isSome

theorem covered_cons (k v : Int) (st : List (Int × Int)) (nd : Int)
    (h : covered st nd = true) : covered ((k, v) :: st) nd = true := by
  unfold covered at h ⊢
  rw [List.find?_cons]
  split
  · rfl
  · exact h

theorem covered_head (k v : Int) (st : List (Int × Int)) :
    covered ((k, v) :: st) k = true := by
  unfold covered; rw [List.find?_cons]; simp

theorem not_covered_of_find_none (st : List (Int × Int)) (nd : Int)
    (h : st.find? (fun p => p.fst == nd) = none) : covered st nd = false := by
  unfold covered; rw [h]; rfl

-- status map only grows: every covered node stays covered
def Mono (st st' : List (Int × Int)) : Prop :=
  ∀ nd, covered st nd = true → covered st' nd = true

-- measure: number of universe nodes not yet in the status map
def unvisited (univ : List Int) (st : List (Int × Int)) : Nat :=
  univ.countP (fun nd => !covered st nd)

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

theorem unvisited_le (univ : List Int) (st st' : List (Int × Int)) (hm : Mono st st') :
    unvisited univ st' ≤ unvisited univ st := by
  unfold unvisited
  apply countP_le univ (fun nd => !covered st nd) (fun nd => !covered st' nd)
  intro nd hnd
  cases hc : covered st nd with
  | true  => rw [hm nd hc] at hnd; simp at hnd
  | false => simp [hc]

theorem unvisited_lt (univ : List Int) (st st' : List (Int × Int)) (node : Int)
    (hu : node ∈ univ) (hnc : covered st node = false) (hcov : covered st' node = true)
    (hm : Mono st st') : unvisited univ st' < unvisited univ st := by
  unfold unvisited
  refine countP_lt univ (fun nd => !covered st nd) (fun nd => !covered st' nd)
    ?_ node hu ?_ ?_
  · intro nd hnd
    by_cases hc : covered st nd = true
    · simp [hm nd hc] at hnd
    · simp only [Bool.not_eq_true] at hc; simp [hc]
  · simp [hcov]
  · simp [hnc]

-- DFS, total.  `visit` finishes one node (and everything reachable from it);
-- `visitChildren` is the successor loop.  Both carry the proof that the status
-- map only grows (`Mono`), which the lexicographic measure needs.
mutual
  def visit (univ : List Int) (g : List (Int × List Int))
      (hsub : ∀ x ∈ graphNodes g, x ∈ univ)
      (st : List (Int × Int)) (order : List Int) (node : Int) (hnode : node ∈ univ) :
      { r : Option (List (Int × Int) × List Int) // ∀ st' o', r = some (st', o') → Mono st st' } :=
    match hf : st.find? (fun p => p.fst == node) with
    | some (_, 2) =>
        ⟨some (st, order), by
          intro st' o' he
          simp only [Option.some.injEq, Prod.mk.injEq] at he
          obtain ⟨rfl, rfl⟩ := he
          intro nd h; exact h⟩
    | some _ => ⟨none, by intro st' o' he; simp at he⟩
    | none =>
        let cr := visitChildren univ g hsub ((node, 1) :: st) order (succsOf node g)
                    (fun x hx => hsub x (succ_mem_graphNodes g node x hx))
        match hc : cr.val with
        | none => ⟨none, by intro st' o' he; simp at he⟩
        | some (st2, order2) =>
            ⟨some ((node, 2) :: st2, node :: order2), by
              intro st' o' he
              simp only [Option.some.injEq, Prod.mk.injEq] at he
              obtain ⟨rfl, rfl⟩ := he
              have hcm : Mono ((node, 1) :: st) st2 := cr.property st2 order2 hc
              intro nd h
              exact covered_cons node 2 st2 nd (hcm nd (covered_cons node 1 st nd h))⟩
  termination_by (unvisited univ st, 0)
  decreasing_by
    simp_wf
    exact Prod.Lex.left _ _ (unvisited_lt univ st ((node, 1) :: st) node hnode
      (not_covered_of_find_none st node hf) (covered_head node 1 st)
      (fun nd h => covered_cons node 1 st nd h))

  def visitChildren (univ : List Int) (g : List (Int × List Int))
      (hsub : ∀ x ∈ graphNodes g, x ∈ univ)
      (st : List (Int × Int)) (order : List Int) (succs : List Int)
      (hsuccs : ∀ s ∈ succs, s ∈ univ) :
      { r : Option (List (Int × Int) × List Int) // ∀ st' o', r = some (st', o') → Mono st st' } :=
    match succs with
    | [] =>
        ⟨some (st, order), by
          intro st' o' he
          simp only [Option.some.injEq, Prod.mk.injEq] at he
          obtain ⟨rfl, rfl⟩ := he
          intro nd h; exact h⟩
    | s :: rest =>
        let vr := visit univ g hsub st order s (hsuccs s List.mem_cons_self)
        match hv : vr.val with
        | none => ⟨none, by intro st' o' he; simp at he⟩
        | some (st', order') =>
            let rr := visitChildren univ g hsub st' order' rest
                        (fun x hx => hsuccs x (List.mem_cons_of_mem _ hx))
            ⟨rr.val, by
              intro st'' o'' he
              have hvm : Mono st st' := vr.property st' order' hv
              have hrm : Mono st' st'' := rr.property st'' o'' he
              intro nd h
              exact hrm nd (hvm nd h)⟩
  termination_by (unvisited univ st, succs.length + 1)
  decreasing_by
    all_goals simp_wf
    · exact Prod.Lex.right _ (by omega)
    · have hvm : Mono st st' := vr.property st' order' hv
      have hle := unvisited_le univ st st' hvm
      rcases Nat.lt_or_eq_of_le hle with hlt | heq
      · exact Prod.Lex.left _ _ hlt
      · rw [heq]; exact Prod.Lex.right _ (by omega)
end

def findOrder (n : Nat) (prereqs : List (List Int)) : List Int :=
  let g     := buildGraph prereqs
  let nodes := (List.range n).map (fun i => (Int.ofNat i))
  let univ  := nodes ++ graphNodes g
  let hsub : ∀ x ∈ graphNodes g, x ∈ univ := fun x hx => List.mem_append_right nodes hx
  let result := nodes.attach.foldl
    (fun acc (np : { x // x ∈ nodes }) =>
      match acc with
      | none            => none
      | some (st, order) =>
        (visit univ g hsub st order np.val (List.mem_append_left (graphNodes g) np.property)).val)
    (some ([], []))
  match result with
  | some (_, order) => order
  | none            => []

#guard findOrder 4 [[1,0],[2,0],[3,1],[3,2]] == [0, 1, 2, 3]  -- a valid topological order
#guard findOrder 2 [[1,0],[0,1]]             == []
