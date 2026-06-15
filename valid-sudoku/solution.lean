/-
LeetCode 36: Valid Sudoku
Rows, columns, and 3x3 boxes must have no duplicate non-'.' digits.
Lean 4, core library only.
-/

def noDups (xs : List Char) : Bool :=
  let ds := xs.filter (· != '.')
  let rec uniq : List Char → Bool
    | []      => true
    | x :: rest => !rest.contains x && uniq rest
  uniq ds

def chunks3 : List α → List (List α)
  | [] => []
  | x :: rest => (x :: rest).take 3 :: chunks3 ((x :: rest).drop 3)
  termination_by l => l.length
  decreasing_by all_goals simp_wf; all_goals omega

-- total element count, as an explicit measure (opaque to `simp_wf`, so the
-- termination goal stays in `totalLen _ < totalLen _` form)
def totalLen {α} : List (List α) → Nat
  | []      => 0
  | r :: rs => r.length + totalLen rs

-- one transpose step never grows the total element count …
theorem totalLen_tail_le {α} (xs : List (List α)) :
    totalLen (xs.map List.tail) ≤ totalLen xs := by
  induction xs with
  | nil => simp [totalLen]
  | cons r rs ih =>
    simp only [List.map_cons, totalLen]
    have hr : (List.tail r).length ≤ r.length := by cases r <;> simp
    omega

-- … and strictly shrinks it while some row is non-empty (there is a head)
theorem totalLen_tail_lt {α} (xs : List (List α)) (h : xs.filterMap List.head? ≠ []) :
    totalLen (xs.map List.tail) < totalLen xs := by
  induction xs with
  | nil => simp at h
  | cons r rs ih =>
    cases r with
    | nil =>
      have hrec := ih (by simpa using h)
      simp only [List.map_cons, totalLen, List.tail_nil, List.length_nil]
      omega
    | cons a r' =>
      have hle := totalLen_tail_le rs
      simp only [List.map_cons, totalLen, List.tail_cons, List.length_cons]
      omega

-- transpose for a rectangular list of lists.  Total: each step drops the row
-- heads, strictly shrinking the total element count while any row is non-empty.
def transpose : List (List α) → List (List α)
  | [] => []
  | x :: xs =>
    if h : ((x :: xs).filterMap List.head?).isEmpty then []
    else (x :: xs).filterMap List.head? :: transpose ((x :: xs).map List.tail)
  termination_by l => totalLen l
  decreasing_by
    simp_wf
    refine totalLen_tail_lt (x :: xs) ?_
    intro he
    simp [he] at h

-- the three 3x3 boxes spanning a band of 3 rows
def bandBoxes (rows : List (List Char)) : List (List Char) :=
  (transpose (rows.map chunks3)).map List.flatten

def boxes (board : List (List Char)) : List (List Char) :=
  (chunks3 board).flatMap bandBoxes

def isValidSudoku (board : List (List Char)) : Bool :=
  (board ++ transpose board ++ boxes board).all noDups

def mkBoard (rows : List String) : List (List Char) := rows.map String.toList

def validBoard : List (List Char) := mkBoard
  ["53..7....", "6..195...", ".98....6.", "8...6...3", "4..8.3..1",
   "7...2...6", ".6....28.", "...419..5", "....8..79"]

def invalidBoard : List (List Char) := mkBoard
  ["83..7....", "6..195...", ".98....6.", "8...6...3", "4..8.3..1",
   "7...2...6", ".6....28.", "...419..5", "....8..79"]

#guard isValidSudoku validBoard == true
#guard isValidSudoku invalidBoard == false
