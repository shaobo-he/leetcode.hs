module solution where

open import Data.Nat using (ℕ; zero; suc)
open import Data.List using (List; []; _∷_; map; _++_; length)
open import Data.Char using (Char)
open import Data.String using (String; fromList)
open import Data.Product using (Σ; _×_; _,_; ∃; ∃-syntax)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Grammar of valid parentheses, generated structurally.
-- State: `o` = opens still to place, `d` = current nesting depth.
-- Recursion is structural (on o, then on d), so Agda accepts it as total.
genB : ℕ → ℕ → List (List Char)
genB zero    zero     = [] ∷ []
genB zero    (suc d') = map (')' ∷_) (genB zero d')
genB (suc o') zero    = map ('(' ∷_) (genB o' 1)
genB (suc o') (suc d') = map ('(' ∷_) (genB o' (suc (suc d')))
                      ++ map (')' ∷_) (genB (suc o') d')

generateParenthesis : ℕ → List String
generateParenthesis n = map fromList (genB n zero)

-- compile-time tests: same counts the other languages assert
-- generateParenthesis 3 has 5 results, generateParenthesis 4 has 14 (Catalan).
_ : length (generateParenthesis 3) ≡ 5
_ = refl

_ : length (generateParenthesis 4) ≡ 14
_ = refl

-- exact char-list output for n = 2 (matches the genB enumeration order)
_ : genB 2 0 ≡ ('(' ∷ '(' ∷ ')' ∷ ')' ∷ [])
              ∷ ('(' ∷ ')' ∷ '(' ∷ ')' ∷ [])
              ∷ []
_ = refl

------------------------------------------------------------------------
-- Proof that genB produces *exactly* the balanced strings.  Port of the
-- Lean development: `Bal`, `genB_sound`, `genB_complete`, `opens`, and the
-- d = 0 corollaries.  `genB` above is genuinely structural (lexicographic
-- descent on (o, d)), so no TERMINATING pragma is used or needed.
------------------------------------------------------------------------

-- balancedness spec (depth-indexed): `Bal d cs` = reading cs from depth d
-- returns to 0 without going negative; `Bal 0` is "well-formed parentheses".
data Bal : ℕ → List Char → Set where
  nil : Bal zero []
  opn : ∀ {d xs} → Bal (suc d) xs → Bal d ('(' ∷ xs)
  cls : ∀ {d xs} → Bal d xs → Bal (suc d) (')' ∷ xs)

-- number of '(' in a string
opens : List Char → ℕ
opens []           = zero
opens ('(' ∷ cs)   = suc (opens cs)
opens (_   ∷ cs)   = opens cs

-- our own propositional list membership (here / there)
infix 4 _∈_
data _∈_ {A : Set} (x : A) : List A → Set where
  here  : ∀ {xs}          → x ∈ (x ∷ xs)
  there : ∀ {y xs} → x ∈ xs → x ∈ (y ∷ xs)

-- membership through `map`, both directions
∈-map⁺ : ∀ {A B : Set} (f : A → B) {x xs} → x ∈ xs → f x ∈ map f xs
∈-map⁺ f here      = here
∈-map⁺ f (there p) = there (∈-map⁺ f p)

∈-map⁻ : ∀ {A B : Set} (f : A → B) {y xs} →
         y ∈ map f xs → ∃[ x ] (x ∈ xs × y ≡ f x)
∈-map⁻ f {xs = x ∷ xs} here      = x , here , refl
∈-map⁻ f {xs = x ∷ xs} (there p) with ∈-map⁻ f p
... | x' , p' , eq = x' , there p' , eq

-- membership through `_++_`
∈-++⁺ˡ : ∀ {A : Set} {x : A} {xs ys} → x ∈ xs → x ∈ (xs ++ ys)
∈-++⁺ˡ here      = here
∈-++⁺ˡ (there p) = there (∈-++⁺ˡ p)

∈-++⁺ʳ : ∀ {A : Set} {x : A} (xs : List A) {ys} → x ∈ ys → x ∈ (xs ++ ys)
∈-++⁺ʳ []       p = p
∈-++⁺ʳ (_ ∷ xs) p = there (∈-++⁺ʳ xs p)

∈-++⁻ : ∀ {A : Set} {x : A} (xs : List A) {ys} →
        x ∈ (xs ++ ys) → (x ∈ xs) ⊎ (x ∈ ys)
∈-++⁻ []       p         = inj₂ p
∈-++⁻ (_ ∷ xs) here      = inj₁ here
∈-++⁻ (_ ∷ xs) (there p) with ∈-++⁻ xs p
... | inj₁ q = inj₁ (there q)
... | inj₂ q = inj₂ q

------------------------------------------------------------------------
-- SOUNDNESS: every generated char-list is balanced at depth d.
-- Recursion mirrors genB's structure, so it is structurally terminating.
------------------------------------------------------------------------

genB-sound : ∀ (o d : ℕ) (cs : List Char) → cs ∈ genB o d → Bal d cs
genB-sound zero    zero    cs       h with h
... | here = nil
genB-sound zero    (suc d') cs      h with ∈-map⁻ (')' ∷_) h
... | cs' , h' , refl = cls (genB-sound zero d' cs' h')
genB-sound (suc o') zero   cs       h with ∈-map⁻ ('(' ∷_) h
... | cs' , h' , refl = opn (genB-sound o' 1 cs' h')
genB-sound (suc o') (suc d') cs     h
  with ∈-++⁻ (map ('(' ∷_) (genB o' (suc (suc d')))) h
... | inj₁ hl with ∈-map⁻ ('(' ∷_) hl
...   | cs' , h' , refl = opn (genB-sound o' (suc (suc d')) cs' h')
genB-sound (suc o') (suc d') cs     h
    | inj₂ hr with ∈-map⁻ (')' ∷_) hr
...   | cs' , h' , refl = cls (genB-sound (suc o') d' cs' h')

------------------------------------------------------------------------
-- COMPLETENESS: every balanced char-list is generated (at its open-count).
-- Induction on the Bal derivation.  The head of cs is pinned to '(' or ')'
-- by the Bal constructor, so the `opens` catch-all over a non-'(' char is
-- definitional: opens (')' ∷ xs) reduces to opens xs.
------------------------------------------------------------------------

genB-complete : ∀ (cs : List Char) (d : ℕ) → Bal d cs → cs ∈ genB (opens cs) d
genB-complete .[] .zero nil = here
-- '(' case: opens ('(' ∷ xs) = suc (opens xs); split on d.
genB-complete .('(' ∷ xs) d (opn {d} {xs} p) with d
... | zero  = ∈-map⁺ ('(' ∷_) (genB-complete xs 1 p)
... | suc d' = ∈-++⁺ˡ (∈-map⁺ ('(' ∷_) (genB-complete xs (suc (suc d')) p))
-- ')' case: opens (')' ∷ xs) = opens xs (definitionally); split on opens xs.
-- We abstract `opens xs` and the recursive proof simultaneously, so the proof's
-- type tracks each branch (opens xs = 0 vs suc o).
genB-complete .(')' ∷ xs) (suc d) (cls {d} {xs} p)
  with opens xs | genB-complete xs d p
... | zero  | rec = ∈-map⁺ (')' ∷_) rec
... | suc o | rec = ∈-++⁺ʳ (map ('(' ∷_) (genB o (suc (suc d))))
                           (∈-map⁺ (')' ∷_) rec)

------------------------------------------------------------------------
-- Corollaries at d = 0: the actual answer (genB n 0) consists of balanced
-- strings, and every balanced char-list with n opens shows up.
------------------------------------------------------------------------

generate-sound : ∀ (n : ℕ) (cs : List Char) → cs ∈ genB n zero → Bal zero cs
generate-sound n cs h = genB-sound n zero cs h

generate-complete : ∀ (cs : List Char) → Bal zero cs → cs ∈ genB (opens cs) zero
generate-complete cs h = genB-complete cs zero h

-- non-vacuity check: "(())" is balanced (and ")(", "(((" are not derivable).
example-balanced : Bal zero ('(' ∷ '(' ∷ ')' ∷ ')' ∷ [])
example-balanced = opn (opn (cls (cls nil)))
