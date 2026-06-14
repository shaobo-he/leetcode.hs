module solution where

-- Valid parentheses.
-- Push the expected closer for each opener; match each closer against the
-- stack top. Valid exactly when every closer matches and the stack empties.

open import Data.Bool using (Bool; true; false; if_then_else_; _∨_)
open import Data.Char using (Char; _==_; _≟_)
open import Data.List using (List; []; _∷_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Product using (_×_; _,_)
open import Data.String using (String; toList)
open import Relation.Nullary using (Dec; yes; no)
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

------------------------------------------------------------------------
-- Formal verification (single bracket type, as in the Lean/Idris proofs).
--
-- `Bal d cs` = reading cs from depth d returns to 0 without going negative;
-- `Bal 0` is "well-formed parentheses".  `check` is the depth-counter
-- recognizer: `isValid` specialised to one kind of bracket with the
-- stack-of-closers collapsed into a depth counter.  We prove the recognizer
-- accepts EXACTLY the balanced strings — a port of Lean's `check_iff_bal`
-- and the corollary `accepts_iff_balanced`.
--
-- We use the single-bracket '(' / ')' model over the primitive `Char`,
-- branching `check` on the two literals plus a catch-all (as Lean's `check`),
-- via decidable Char equality `_≟_`.
------------------------------------------------------------------------

-- The depth-indexed balanced-parens spec (mirrors Lean/Idris `Bal`).
data Bal : ℕ → List Char → Set where
  nil : Bal 0 []
  opn : ∀ {d xs} → Bal (suc d) xs → Bal d ('(' ∷ xs)
  cls : ∀ {d xs} → Bal d xs       → Bal (suc d) (')' ∷ xs)

-- The depth-counter recognizer.  We branch the head character on whether it is
-- '(' (descend at suc depth), ')' (ascend, failing at depth 0), or neither
-- (reject) — the same three cases as Lean's `check` — via decidable Char
-- equality.  The decisions are dispatched through `checkHead`, with the list
-- split first so that `check d (c ∷ cs)` reduces definitionally to the
-- `checkHead` call (this exposes the recognizer's behaviour to the proofs).
check     : ℕ → List Char → Bool
checkHead : (d : ℕ) (c : Char) → List Char
          → Dec (c ≡ '(') → Dec (c ≡ ')') → Bool

check d []       = checkNil d
  where checkNil : ℕ → Bool
        checkNil zero    = true
        checkNil (suc _) = false
check d (c ∷ cs) = checkHead d c cs (c ≟ '(') (c ≟ ')')

checkHead d       c cs (yes _) _       = check (suc d) cs   -- '(' : descend
checkHead d       c cs (no _)  (no _)  = false              -- neither bracket
checkHead zero    c cs (no _)  (yes _) = false              -- ')' at depth 0
checkHead (suc d) c cs (no _)  (yes _) = check d cs         -- ')' : ascend

-- Soundness: every accepted string is balanced (check d cs ≡ true → Bal d cs).
checkSound     : (d : ℕ) (cs : List Char) → check d cs ≡ true → Bal d cs
checkSoundHead : (d : ℕ) (c : Char) (cs : List Char)
                 (p : Dec (c ≡ '(')) (q : Dec (c ≡ ')'))
               → checkHead d c cs p q ≡ true → Bal d (c ∷ cs)

checkSound zero    []       _   = nil
checkSound (suc _) []       ()
checkSound d (c ∷ cs) prf = checkSoundHead d c cs (c ≟ '(') (c ≟ ')') prf

checkSoundHead d       c cs (yes refl) q         prf = opn (checkSound (suc d) cs prf)
checkSoundHead d       c cs (no _)     (no _)    ()                  -- false ≡ true impossible
checkSoundHead zero    c cs (no _)     (yes _)   ()                  -- false ≡ true impossible
checkSoundHead (suc d) c cs (no _)     (yes refl) prf = cls (checkSound d cs prf)

-- Completeness: every balanced string is accepted (Bal d cs → check d cs ≡ true).
checkComplete : (d : ℕ) (cs : List Char) → Bal d cs → check d cs ≡ true
checkComplete zero    []          nil     = refl
checkComplete d       ('(' ∷ xs)  (opn b) = checkComplete (suc d) xs b
checkComplete (suc d) (')' ∷ xs)  (cls b) = checkComplete d xs b

-- The two directions packaged as an iff (⇔ = pair of functions).
check_iff_bal : (d : ℕ) (cs : List Char)
              → (check d cs ≡ true → Bal d cs) × (Bal d cs → check d cs ≡ true)
check_iff_bal d cs = checkSound d cs , checkComplete d cs

-- Corollary: `check 0` accepts a string iff it is balanced (Bal 0).
accepts_iff_balanced : (cs : List Char)
                     → (check 0 cs ≡ true → Bal 0 cs) × (Bal 0 cs → check 0 cs ≡ true)
accepts_iff_balanced cs = check_iff_bal 0 cs
