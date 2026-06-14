-- Valid parentheses.
-- Push the expected closer for each opener; match each closer against the
-- stack top. Valid exactly when every closer matches and the stack empties.

def closeOf : Char → Char
  | '(' => ')'
  | '[' => ']'
  | '{' => '}'
  | c   => c

def isOpen (c : Char) : Bool :=
  c == '(' || c == '[' || c == '{'

def isValid (s : String) : Bool :=
  let rec go : List Char → List Char → Bool
    | [],      []      => true
    | [],      _ :: _  => false
    | c :: cs, stack   =>
      if isOpen c then
        go cs (closeOf c :: stack)
      else
        match stack with
        | top :: rest => if c == top then go cs rest else false
        | []          => false
  go s.toList []

#guard isValid "()[]{}" == true
#guard isValid "(]" == false
#guard isValid "{[]}" == true

/- ─────────────────────────────────────────────────────────────────────────
   Formal verification (single bracket type, as in the Idris proof):
   `Bal d cs` = reading cs from depth d returns to 0 without going negative;
   `Bal 0` is "well-formed parentheses".  We prove the depth-counter recognizer
   `check 0` accepts EXACTLY the balanced strings, with Lean tactics.
   ───────────────────────────────────────────────────────────────────────── -/

inductive Bal : Nat → List Char → Prop
  | nil  : Bal 0 []
  | opn  : Bal (d + 1) xs → Bal d ('(' :: xs)
  | cls  : Bal d xs → Bal (d + 1) (')' :: xs)

def check : Nat → List Char → Bool
  | 0,     []        => true
  | _ + 1, []        => false
  | d,     '(' :: cs => check (d + 1) cs
  | 0,     ')' :: _  => false
  | d + 1, ')' :: cs => check d cs
  | _,     _ :: _    => false

theorem check_iff_bal (d : Nat) (cs : List Char) : check d cs = true ↔ Bal d cs := by
  induction d, cs using check.induct with
  | case1 => exact ⟨fun _ => .nil, fun _ => rfl⟩                 -- 0, []
  | case2 d =>                                                    -- d+1, []
      constructor
      · intro h; simp [check] at h
      · intro h; cases h
  | case3 d cs ih =>                                              -- d, '(' :: cs
      simp only [check]; rw [ih]
      exact ⟨.opn, fun h => by cases h with | opn p => exact p⟩
  | case4 cs =>                                                   -- 0, ')' :: cs
      constructor
      · intro h; simp [check] at h
      · intro h; cases h
  | case5 d cs ih =>                                              -- d+1, ')' :: cs
      simp only [check]; rw [ih]
      exact ⟨.cls, fun h => by cases h with | cls p => exact p⟩
  | case6 =>                                                     -- d, c :: cs (c not a bracket)
      simp only [check]
      constructor
      · intro h; simp at h
      · intro h; cases h <;> simp_all

-- corollary: `check 0` accepts a string iff it is balanced
theorem accepts_iff_balanced (cs : List Char) : check 0 cs = true ↔ Bal 0 cs :=
  check_iff_bal 0 cs
