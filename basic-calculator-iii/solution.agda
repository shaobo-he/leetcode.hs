module solution where

-- Recursive-descent calculator (port of the Lean/Idris solution).
-- expr = term (('+'|'-') term)* ; term = factor (('*'|'/') factor)*
-- factor = '(' expr ')' | number
-- Values are integers (ℤ) because subexpressions can be negative, e.g. (0-3)/2.
--
-- TOTAL, no TERMINATING pragma.  The grammar consumes the input across mutually
-- recursive calls, so it is not structurally recursive; instead we recurse on a
-- single-ℕ lexicographic measure `rank + length input * 6` (rank-first so `suc`
-- reduces definitionally): parseExpr 5 > exprTail 4 > parseTerm 3 > termTail 2 >
-- parseFactor 1.  Each parser returns its remainder paired with a proof
-- `length remainder ≤ length input`; the same-input forward calls drop the rank,
-- the loops and the '('-recursion drop the length using that carried bound.  The
-- measure values m,n,a,b are passed explicitly because `+ℕ`/`*ℕ` are not
-- injective, so Agda cannot recover them by unification.

open import Data.Bool using (Bool; true; false; _xor_; if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _∸_; _<_; _≤_; s≤s; z≤n; _≡ᵇ_)
  renaming (_*_ to _*ℕ_; _+_ to _+ℕ_; _/_ to _/ℕ_)
open import Data.Nat.Properties using (≤-refl; ≤-trans; n≤1+n;
  *-monoˡ-≤; +-monoˡ-≤; +-monoʳ-≤; m≤n+m)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.Integer using (ℤ; +_; -[1+_]; _+_; _-_; _*_; ∣_∣; -_)
open import Data.Char using (Char; isDigit; toℕ)
open import Data.List using (List; []; _∷_; length)
open import Data.String using (String; toList)
open import Data.Product using (_×_; _,_; proj₁; proj₂; Σ; Σ-syntax)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- ─── measure-decrease lemmas (rank ∈ {1..5}, so 6 dominates a rank bump) ───
-- m,n,a,b explicit: `+ℕ`/`*ℕ` aren't injective, so they can't be inferred.

-- strictly shorter input dominates any rank (a ≤ 5)
lenStep : ∀ m n a b → suc m ≤ n → a ≤ 5 → suc (a +ℕ m *ℕ 6) ≤ b +ℕ n *ℕ 6
lenStep m n a b sm≤n a≤5 =
  ≤-trans (+-monoˡ-≤ (m *ℕ 6) (s≤s a≤5))
          (≤-trans (*-monoˡ-≤ 6 sm≤n) (m≤n+m (n *ℕ 6) b))

-- same-or-shorter input, strictly smaller rank
rankStep : ∀ m n a b → m ≤ n → suc a ≤ b → suc (a +ℕ m *ℕ 6) ≤ b +ℕ n *ℕ 6
rankStep m n a b m≤n sa≤b =
  ≤-trans (+-monoˡ-≤ (m *ℕ 6) sa≤b)
          (+-monoʳ-≤ b (*-monoˡ-≤ 6 m≤n))

-- concrete rank-order facts
1≤5 : 1 ≤ 5
1≤5 = s≤s z≤n
2≤5 : 2 ≤ 5
2≤5 = s≤s (s≤s z≤n)
3≤5 : 3 ≤ 5
3≤5 = s≤s (s≤s (s≤s z≤n))
4≤5 : 4 ≤ 5
4≤5 = s≤s (s≤s (s≤s (s≤s z≤n)))
2≤3 : 2 ≤ 3
2≤3 = s≤s (s≤s z≤n)

-- ─── lexing helpers ───

isSpace : Char → Bool
isSpace c = toℕ c ≡ᵇ toℕ ' '

dropSpaces : List Char → List Char
dropSpaces []       = []
dropSpaces (c ∷ cs) = if isSpace c then dropSpaces cs else c ∷ cs

dropSpaces-len : (cs : List Char) → length (dropSpaces cs) ≤ length cs
dropSpaces-len []       = ≤-refl
dropSpaces-len (c ∷ cs) with isSpace c
... | true  = ≤-trans (dropSpaces-len cs) (n≤1+n (length cs))
... | false = ≤-refl

-- natural division returning 0 on a zero divisor (avoids a NonZero obligation;
-- the divisor is never zero in well-formed input).
divN : ℕ → ℕ → ℕ
divN a zero    = zero
divN a (suc m) = a /ℕ (suc m)

isNeg : ℤ → Bool
isNeg (+_ _)   = false
isNeg -[1+ _ ] = true

-- integer division truncated toward zero (LeetCode semantics)
tdiv : ℤ → ℤ → ℤ
tdiv a b = let q = + (divN ∣ a ∣ ∣ b ∣) in
           if (isNeg a) xor (isNeg b) then - q else q

charDigit : Char → ℕ
charDigit c = toℕ c ∸ toℕ '0'

-- parse a non-negative integer literal
parseDigits : ℕ → List Char → ℕ × List Char
parseDigits accu []       = accu , []
parseDigits accu (c ∷ cs) with isDigit c
... | true  = parseDigits (accu *ℕ 10 +ℕ charDigit c) cs
... | false = accu , (c ∷ cs)

parseDigits-len : (accu : ℕ) (cs : List Char) →
    length (proj₂ (parseDigits accu cs)) ≤ length cs
parseDigits-len accu []       = ≤-refl
parseDigits-len accu (c ∷ cs) with isDigit c
... | true  = ≤-trans (parseDigits-len (accu *ℕ 10 +ℕ charDigit c) cs) (n≤1+n (length cs))
... | false = ≤-refl

parseNumber : List Char → ℤ × List Char
parseNumber cs = (+ proj₁ (parseDigits 0 cs)) , proj₂ (parseDigits 0 cs)

parseNumber-len : (cs : List Char) → length (proj₂ (parseNumber cs)) ≤ length cs
parseNumber-len cs = parseDigits-len 0 cs

-- ─── result type: value + remainder bounded by the measure's length ───

Bounded : ℕ → Set
Bounded n = ℤ × Σ[ r ∈ List Char ] (length r ≤ n)

Res : List Char → Set
Res input = Bounded (length input)

val : ∀ {n} → Bounded n → ℤ
val = proj₁

rem : ∀ {n} → Bounded n → List Char
rem r = proj₁ (proj₂ r)

remB : ∀ {n} → (r : Bounded n) → length (rem r) ≤ n
remB r = proj₂ (proj₂ r)

-- consume a matching ')' if present (non-recursive, so literal patterns are fine)
consumeParen : ∀ {n} → ℤ → (xs : List Char) → length xs ≤ n → Bounded n
consumeParen v (')' ∷ rest) h = v , (rest , ≤-trans (n≤1+n (length rest)) h)
consumeParen v other        h = v , (other , h)

-- ─── recursive-descent core, total by well-founded recursion on the measure ───

mutual
  parseExpr : (cs : List Char) → Acc _<_ (5 +ℕ length cs *ℕ 6) → Res cs
  parseExpr cs (acc rec) =
    let r1  = parseTerm cs (rec (rankStep (length cs) (length cs) 3 5 ≤-refl 4≤5))
        ds  = dropSpaces (rem r1)
        dsB = ≤-trans (dropSpaces-len (rem r1)) (remB r1)
        r2  = exprTail (val r1) ds (rec (rankStep (length ds) (length cs) 4 5 dsB ≤-refl))
    in val r2 , (rem r2 , ≤-trans (remB r2) dsB)

  exprTail : (a : ℤ) → (input : List Char) →
      Acc _<_ (4 +ℕ length input *ℕ 6) → Res input
  exprTail a ('+' ∷ cs) (acc rec) =
    let r1  = parseTerm cs (rec (lenStep (length cs) (suc (length cs)) 3 4 ≤-refl 3≤5))
        ds  = dropSpaces (rem r1)
        dsB = ≤-trans (dropSpaces-len (rem r1)) (remB r1)
        r2  = exprTail (a + val r1) ds (rec (lenStep (length ds) (suc (length cs)) 4 4 (s≤s dsB) 4≤5))
    in val r2 , (rem r2 , ≤-trans (remB r2) (≤-trans dsB (n≤1+n (length cs))))
  exprTail a ('-' ∷ cs) (acc rec) =
    let r1  = parseTerm cs (rec (lenStep (length cs) (suc (length cs)) 3 4 ≤-refl 3≤5))
        ds  = dropSpaces (rem r1)
        dsB = ≤-trans (dropSpaces-len (rem r1)) (remB r1)
        r2  = exprTail (a - val r1) ds (rec (lenStep (length ds) (suc (length cs)) 4 4 (s≤s dsB) 4≤5))
    in val r2 , (rem r2 , ≤-trans (remB r2) (≤-trans dsB (n≤1+n (length cs))))
  exprTail a cs _ = a , (cs , ≤-refl)

  parseTerm : (cs : List Char) → Acc _<_ (3 +ℕ length cs *ℕ 6) → Res cs
  parseTerm cs (acc rec) =
    let r1  = parseFactor cs (rec (rankStep (length cs) (length cs) 1 3 ≤-refl 2≤3))
        ds  = dropSpaces (rem r1)
        dsB = ≤-trans (dropSpaces-len (rem r1)) (remB r1)
        r2  = termTail (val r1) ds (rec (rankStep (length ds) (length cs) 2 3 dsB ≤-refl))
    in val r2 , (rem r2 , ≤-trans (remB r2) dsB)

  termTail : (a : ℤ) → (input : List Char) →
      Acc _<_ (2 +ℕ length input *ℕ 6) → Res input
  termTail a ('*' ∷ cs) (acc rec) =
    let r1  = parseFactor cs (rec (lenStep (length cs) (suc (length cs)) 1 2 ≤-refl 1≤5))
        ds  = dropSpaces (rem r1)
        dsB = ≤-trans (dropSpaces-len (rem r1)) (remB r1)
        r2  = termTail (a * val r1) ds (rec (lenStep (length ds) (suc (length cs)) 2 2 (s≤s dsB) 2≤5))
    in val r2 , (rem r2 , ≤-trans (remB r2) (≤-trans dsB (n≤1+n (length cs))))
  termTail a ('/' ∷ cs) (acc rec) =
    let r1  = parseFactor cs (rec (lenStep (length cs) (suc (length cs)) 1 2 ≤-refl 1≤5))
        ds  = dropSpaces (rem r1)
        dsB = ≤-trans (dropSpaces-len (rem r1)) (remB r1)
        r2  = termTail (tdiv a (val r1)) ds (rec (lenStep (length ds) (suc (length cs)) 2 2 (s≤s dsB) 2≤5))
    in val r2 , (rem r2 , ≤-trans (remB r2) (≤-trans dsB (n≤1+n (length cs))))
  termTail a cs _ = a , (cs , ≤-refl)

  parseFactor : (cs0 : List Char) → Acc _<_ (1 +ℕ length cs0 *ℕ 6) → Res cs0
  parseFactor cs0 (acc rec) with dropSpaces cs0 | dropSpaces-len cs0
  ... | '(' ∷ rest | hlen =
          let r1 = parseExpr rest (rec (lenStep (length rest) (length cs0) 5 1 hlen ≤-refl))
              b1 = ≤-trans (remB r1) (≤-trans (n≤1+n (length rest)) hlen)
          in consumeParen (val r1) (dropSpaces (rem r1))
               (≤-trans (dropSpaces-len (rem r1)) b1)
  ... | cs | hlen =
          proj₁ (parseNumber cs) ,
          (proj₂ (parseNumber cs) , ≤-trans (parseNumber-len cs) hlen)

calculate : String → ℤ
calculate s = val (parseExpr (toList s) (<-wellFounded (5 +ℕ length (toList s) *ℕ 6)))

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : calculate "1+1" ≡ + 2
_ = refl

_ : calculate "6-4/2" ≡ + 4
_ = refl

_ : calculate "2*(5+5*2)+3*5" ≡ + 45
_ = refl

_ : calculate "14-3/2" ≡ + 13
_ = refl

_ : calculate "(0-3)/2" ≡ (+ 0 - + 1)
_ = refl
