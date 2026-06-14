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

partial def chunks3 : List α → List (List α)
  | [] => []
  | xs => xs.take 3 :: chunks3 (xs.drop 3)

-- transpose for a rectangular list of lists
partial def transpose : List (List α) → List (List α)
  | [] => []
  | xs =>
    let heads := xs.filterMap List.head?
    if heads.isEmpty then []
    else heads :: transpose (xs.map List.tail)

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
