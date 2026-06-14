module solution where

-- Valid parentheses.
-- Push the expected closer for each opener; match each closer against the
-- stack top. Valid exactly when every closer matches and the stack empties.

open import Data.Bool using (Bool; true; false; if_then_else_; _∨_)
open import Data.Char using (Char; _==_)
open import Data.List using (List; []; _∷_)
open import Data.String using (String; toList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

closeOf : Char → Char
closeOf c =
  if c == '(' then ')'
  else if c == '[' then ']'
  else if c == '{' then '}'
  else c

isOpen : Char → Bool
isOpen c = (c == '(') ∨ (c == '[') ∨ (c == '{')

-- Push the expected closer for each opener; match closers against the stack top.
go : List Char → List Char → Bool
go []       []        = true
go []       (_ ∷ _)   = false
go (c ∷ cs) stack =
  if isOpen c
    then go cs (closeOf c ∷ stack)
    else (case-stack c cs stack)
  where
    case-stack : Char → List Char → List Char → Bool
    case-stack c cs []          = false
    case-stack c cs (top ∷ rest) = if c == top then go cs rest else false

isValid : String → Bool
isValid s = go (toList s) []

-- compile-time tests (same examples the other languages test)
_ : isValid "()[]{}" ≡ true
_ = refl

_ : isValid "(]" ≡ false
_ = refl

_ : isValid "{[]}" ≡ true
_ = refl
