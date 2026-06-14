module solution where

-- LeetCode 36: Valid Sudoku
-- Rows, columns, and 3x3 boxes must have no duplicate non-'.' digits.

open import Data.Nat using (ℕ)
open import Data.Bool using (Bool; true; false; not; _∧_; _∨_)
open import Data.Char using (Char; _==_)
open import Data.String using (String; toList)
open import Data.List using (List; []; _∷_; _++_; map; concat; concatMap;
                             filterᵇ; all; mapMaybe)
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

-- stdlib has no List transpose for a rectangular list of lists.
{-# TERMINATING #-}
transpose : List (List A) → List (List A)
transpose []       = []
transpose ([] ∷ _) = []
transpose rows     = mapMaybe headᵐ rows ∷ transpose (map tailᴸ rows)

-- chunks of 3 (non-structural: splits off a fixed prefix).
take3 : List A → List A
take3 (a ∷ b ∷ c ∷ _) = a ∷ b ∷ c ∷ []
take3 xs              = xs

drop3 : List A → List A
drop3 (_ ∷ _ ∷ _ ∷ xs) = xs
drop3 _                = []

{-# TERMINATING #-}
chunks3 : List A → List (List A)
chunks3 []       = []
chunks3 (x ∷ xs) = take3 (x ∷ xs) ∷ chunks3 (drop3 (x ∷ xs))

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
