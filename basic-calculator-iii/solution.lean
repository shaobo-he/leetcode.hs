-- Recursive-descent calculator.
-- expr = term (('+'|'-') term)* ; term = factor (('*'|'/') factor)*

def isDigit' (c : Char) : Bool := c >= '0' && c <= '9'

def dropSpaces : List Char → List Char
  | ' ' :: cs => dropSpaces cs
  | cs        => cs

-- integer division truncated toward zero (LeetCode semantics)
def tdiv (a b : Int) : Int :=
  let q : Int := Int.ofNat (a.natAbs / b.natAbs)
  if (a < 0) != (b < 0) then -q else q

def charToDigit (c : Char) : Int := Int.ofNat (c.toNat - '0'.toNat)

def parseNumberGo (acc : Int) : List Char → Int × List Char
  | c :: rest => if isDigit' c then parseNumberGo (acc * 10 + charToDigit c) rest else (acc, c :: rest)
  | []        => (acc, [])

def parseNumber (cs : List Char) : Int × List Char := parseNumberGo 0 cs

-- remainder-length bounds for the non-recursive helpers
theorem dropSpaces_len (cs : List Char) : (dropSpaces cs).length ≤ cs.length := by
  fun_induction dropSpaces cs <;> simp_all <;> omega

theorem parseNumberGo_len (acc : Int) (cs : List Char) :
    (parseNumberGo acc cs).2.length ≤ cs.length := by
  fun_induction parseNumberGo acc cs <;> simp_all <;> omega

theorem parseNumber_len (cs : List Char) : (parseNumber cs).2.length ≤ cs.length :=
  parseNumberGo_len 0 cs

-- Recursive-descent expr/term/factor, TOTAL.  Each parser returns its remainder
-- paired with a proof `remainder.length ≤ input.length`.  Termination uses a
-- single-Nat encoding of the lexicographic (length, rank) measure,
-- `input.length * 6 + rank` (parseExpr 5 > exprTail 4 > parseTerm 3 > termTail 2
-- > parseFactor 1): the same-input forward calls drop the rank; the loops and the
-- `(`-recursion drop the length (using the carried bounds).  No fuel, no partial.
mutual
  def parseExpr (cs : List Char) : Int × { r : List Char // r.length ≤ cs.length } :=
    let (t, rest) := parseTerm cs
    let (v, rest2) := exprTail t (dropSpaces rest.val)
    (v, ⟨rest2.val, by have := rest2.property; have := dropSpaces_len rest.val
                       have := rest.property; omega⟩)
  termination_by cs.length * 6 + 5
  decreasing_by
    all_goals simp_wf
    all_goals (try simp only [List.length_cons])
    all_goals first
      | omega
      | (have := dropSpaces_len rest.val; have := rest.property; omega)

  def exprTail (acc : Int) (input : List Char) :
      Int × { r : List Char // r.length ≤ input.length } :=
    match input with
    | '+' :: cs =>
        let (t, rest) := parseTerm cs
        let (v, rest2) := exprTail (acc + t) (dropSpaces rest.val)
        (v, ⟨rest2.val, by simp only [List.length_cons]
                           have := rest2.property; have := dropSpaces_len rest.val
                           have := rest.property; omega⟩)
    | '-' :: cs =>
        let (t, rest) := parseTerm cs
        let (v, rest2) := exprTail (acc - t) (dropSpaces rest.val)
        (v, ⟨rest2.val, by simp only [List.length_cons]
                           have := rest2.property; have := dropSpaces_len rest.val
                           have := rest.property; omega⟩)
    | cs => (acc, ⟨cs, Nat.le_refl _⟩)
  termination_by input.length * 6 + 4
  decreasing_by
    all_goals simp_wf
    all_goals (try simp only [List.length_cons])
    all_goals first
      | omega
      | (have := dropSpaces_len rest.val; have := rest.property; omega)

  def parseTerm (cs : List Char) : Int × { r : List Char // r.length ≤ cs.length } :=
    let (f, rest) := parseFactor cs
    let (v, rest2) := termTail f (dropSpaces rest.val)
    (v, ⟨rest2.val, by have := rest2.property; have := dropSpaces_len rest.val
                       have := rest.property; omega⟩)
  termination_by cs.length * 6 + 3
  decreasing_by
    all_goals simp_wf
    all_goals (try simp only [List.length_cons])
    all_goals first
      | omega
      | (have := dropSpaces_len rest.val; have := rest.property; omega)

  def termTail (acc : Int) (input : List Char) :
      Int × { r : List Char // r.length ≤ input.length } :=
    match input with
    | '*' :: cs =>
        let (f, rest) := parseFactor cs
        let (v, rest2) := termTail (acc * f) (dropSpaces rest.val)
        (v, ⟨rest2.val, by simp only [List.length_cons]
                           have := rest2.property; have := dropSpaces_len rest.val
                           have := rest.property; omega⟩)
    | '/' :: cs =>
        let (f, rest) := parseFactor cs
        let (v, rest2) := termTail (tdiv acc f) (dropSpaces rest.val)
        (v, ⟨rest2.val, by simp only [List.length_cons]
                           have := rest2.property; have := dropSpaces_len rest.val
                           have := rest.property; omega⟩)
    | cs => (acc, ⟨cs, Nat.le_refl _⟩)
  termination_by input.length * 6 + 2
  decreasing_by
    all_goals simp_wf
    all_goals (try simp only [List.length_cons])
    all_goals first
      | omega
      | (have := dropSpaces_len rest.val; have := rest.property; omega)

  def parseFactor (cs0 : List Char) : Int × { r : List Char // r.length ≤ cs0.length } :=
    match h : dropSpaces cs0 with
    | '(' :: rest =>
        let (v, rest1) := parseExpr rest
        match h2 : dropSpaces rest1.val with
        | ')' :: rest2 =>
            (v, ⟨rest2, by have hd := dropSpaces_len cs0; have hd2 := dropSpaces_len rest1.val
                           have := rest1.property; rw [h] at hd; rw [h2] at hd2
                           simp only [List.length_cons] at hd hd2; omega⟩)
        | other =>
            (v, ⟨other, by have hd := dropSpaces_len cs0; have hd2 := dropSpaces_len rest1.val
                           have := rest1.property; rw [h] at hd; rw [h2] at hd2
                           simp only [List.length_cons] at hd; omega⟩)
    | cs =>
        ((parseNumber cs).1, ⟨(parseNumber cs).2,
          by have hd := dropSpaces_len cs0; have := parseNumber_len cs
             rw [h] at hd; omega⟩)
  termination_by cs0.length * 6 + 1
  decreasing_by
    simp_wf
    have hd := dropSpaces_len cs0
    rw [h] at hd
    simp only [List.length_cons] at hd
    omega
end

def calculate (s : String) : Int := (parseExpr s.toList).fst

#guard calculate "1+1"           == 2
#guard calculate "6-4/2"         == 4
#guard calculate "2*(5+5*2)+3*5" == 45
#guard calculate "14-3/2"        == 13
#guard calculate "(0-3)/2"       == -1
