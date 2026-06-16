module solution where

-- LeetCode 36: Valid Sudoku
-- Rows, columns, and 3x3 boxes must have no duplicate non-'.' digits.

open import Data.Nat using (ℕ; suc; _<_)
open import Data.Nat.Properties using (≤-refl)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.Bool using (Bool; true; false; not; _∧_; _∨_)
open import Data.Char using (Char; _==_)
open import Data.String using (String; toList)
open import Data.List using (List; []; _∷_; _++_; map; concat; concatMap;
                             filterᵇ; all; mapMaybe; length)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

private
  variable
    A : Set

headᵐ : List A → Maybe A
headᵐ []      = nothing
headᵐ (x ∷ _) = just x

tailᴸ : List A → List A
tailᴸ []       = []
tailᴸ (_ ∷ xs) = xs

-- stdlib has no List transpose for a rectangular list of lists.  Total by
-- well-founded recursion on the first row's length: each step drops one column
-- (`map tailᴸ`), so that length strictly decreases until a row empties.
firstRow : List (List A) → List A
firstRow []      = []
firstRow (r ∷ _) = r

transpose : List (List A) → List (List A)
transpose rows = go rows (<-wellFounded (length (firstRow rows)))
  where
    go : (rs : List (List A)) → Acc _<_ (length (firstRow rs)) → List (List A)
    go []             _         = []
    go ([] ∷ _)       _         = []
    go ((x ∷ r) ∷ rs) (acc rec) =
      mapMaybe headᵐ ((x ∷ r) ∷ rs) ∷ go (map tailᴸ ((x ∷ r) ∷ rs)) (rec ≤-refl)

-- chunks of 3, structurally recursive: matching three deep makes the recursive
-- call land on the tail `rest`, a genuine sub-term (no TERMINATING needed).
chunks3 : List A → List (List A)
chunks3 []                 = []
chunks3 (a ∷ [])           = (a ∷ []) ∷ []
chunks3 (a ∷ b ∷ [])       = (a ∷ b ∷ []) ∷ []
chunks3 (a ∷ b ∷ c ∷ rest) = (a ∷ b ∷ c ∷ []) ∷ chunks3 rest

-- no duplicate non-'.' digits, using Bool char equality.
contains : Char → List Char → Bool
contains _ []       = false
contains x (y ∷ ys) = (x == y) ∨ contains x ys

uniq : List Char → Bool
uniq []       = true
uniq (x ∷ xs) = not (contains x xs) ∧ uniq xs

noDups : List Char → Bool
noDups xs = uniq (filterᵇ (λ c → not (c == '.')) xs)

-- the three 3x3 boxes spanning a band of 3 rows
bandBoxes : List (List Char) → List (List Char)
bandBoxes rows = map concat (transpose (map chunks3 rows))

boxes : List (List Char) → List (List Char)
boxes board = concatMap bandBoxes (chunks3 board)

isValidSudoku : List (List Char) → Bool
isValidSudoku board = all noDups (board ++ transpose board ++ boxes board)

mkBoard : List String → List (List Char)
mkBoard = map toList

validBoard : List (List Char)
validBoard = mkBoard
  ( "53..7...." ∷ "6..195..." ∷ ".98....6." ∷ "8...6...3" ∷ "4..8.3..1"
  ∷ "7...2...6" ∷ ".6....28." ∷ "...419..5" ∷ "....8..79" ∷ [])

invalidBoard : List (List Char)
invalidBoard = mkBoard
  ( "83..7...." ∷ "6..195..." ∷ ".98....6." ∷ "8...6...3" ∷ "4..8.3..1"
  ∷ "7...2...6" ∷ ".6....28." ∷ "...419..5" ∷ "....8..79" ∷ [])

-- compile-time tests: same examples / expected outputs as the other languages.
_ : isValidSudoku validBoard ≡ true
_ = refl

_ : isValidSudoku invalidBoard ≡ false
_ = refl
