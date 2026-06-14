/-
LeetCode 22: Generate Parentheses — with a Lean proof that the output is
*exactly* the balanced strings: soundness (everything generated is balanced)
and completeness (every balanced string is generated).
Lean 4, core library only.
-/

-- balancedness spec: `Bal d cs` = reading cs from depth d returns to 0 without
-- going negative; `Bal 0` is "well-formed parentheses".
inductive Bal : Nat → List Char → Prop
  | nil : Bal 0 []
  | opn : Bal (d + 1) xs → Bal d ('(' :: xs)
  | cls : Bal d xs → Bal (d + 1) (')' :: xs)

-- generator over char-lists: `o` opens still to place, current nesting depth `d`
def genB : Nat → Nat → List (List Char)
  | 0,      0      => [[]]
  | 0,      d' + 1 => (genB 0 d').map (')' :: ·)
  | o' + 1, 0      => (genB o' 1).map ('(' :: ·)
  | o' + 1, d' + 1 => (genB o' (d' + 2)).map ('(' :: ·) ++ (genB (o' + 1) d').map (')' :: ·)
termination_by o d => (o, d)

def generateParenthesis (n : Nat) : List String :=
  (genB n 0).map String.ofList

#guard (generateParenthesis 3).length == 5
#guard (generateParenthesis 4).length == 14
def sortedStrings (xs : List String) : List String :=
  (xs.map (·.toList)).mergeSort (fun a b => decide (a ≤ b)) |>.map String.ofList
#guard sortedStrings (generateParenthesis 3) ==
  sortedStrings ["((()))", "(()())", "(())()", "()(())", "()()()"]

-- number of '(' in a string
def opens : List Char → Nat
  | []        => 0
  | '(' :: cs => opens cs + 1
  | _ :: cs   => opens cs

-- SOUNDNESS: every generated char-list is balanced at depth d.
theorem genB_sound (o d : Nat) (cs : List Char) (h : cs ∈ genB o d) : Bal d cs := by
  induction o, d using genB.induct generalizing cs with
  | case1 => simp [genB] at h; subst h; exact .nil
  | case2 d ih =>
      simp only [genB, List.mem_map] at h
      obtain ⟨cs', hcs', rfl⟩ := h
      exact .cls (ih cs' hcs')
  | case3 o ih =>
      simp only [genB, List.mem_map] at h
      obtain ⟨cs', hcs', rfl⟩ := h
      exact .opn (ih cs' hcs')
  | case4 o d ih1 ih2 =>
      simp only [genB, List.mem_append, List.mem_map] at h
      rcases h with ⟨cs', hcs', rfl⟩ | ⟨cs', hcs', rfl⟩
      · exact .opn (ih1 cs' hcs')
      · exact .cls (ih2 cs' hcs')

-- COMPLETENESS: every balanced char-list is generated (at its own open-count).
theorem genB_complete (cs : List Char) (d : Nat) (h : Bal d cs) :
    cs ∈ genB (opens cs) d := by
  induction h with
  | nil => simp [genB, opens]
  | @opn d xs _ ih =>
      cases d with
      | zero   => simp only [opens, genB, List.mem_map]; exact ⟨xs, ih, rfl⟩
      | succ d => simp only [opens, genB, List.mem_append, List.mem_map]
                  exact Or.inl ⟨xs, ih, rfl⟩
  | @cls d xs _ ih =>
      cases hop : opens xs with
      | zero =>
          rw [show opens (')' :: xs) = 0 from hop]
          simp only [genB, List.mem_map]
          exact ⟨xs, hop ▸ ih, rfl⟩
      | succ o =>
          rw [show opens (')' :: xs) = o + 1 from hop]
          simp only [genB, List.mem_append, List.mem_map]
          exact Or.inr ⟨xs, hop ▸ ih, rfl⟩

-- corollary: the actual answer (strings) consists of balanced strings, and every
-- balanced char-list with n opens shows up.
theorem generate_sound (n : Nat) (cs : List Char) (h : cs ∈ genB n 0) : Bal 0 cs :=
  genB_sound n 0 cs h

theorem generate_complete (cs : List Char) (h : Bal 0 cs) : cs ∈ genB (opens cs) 0 :=
  genB_complete cs 0 h
