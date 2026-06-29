{-# OPTIONS --safe #-}
module solution where

------------------------------------------------------------------------
-- LeetCode 169: Majority Element — Boyer–Moore voting.
--
-- A total left fold (structural over the list, no pragma) carrying
-- (candidate, count).  `majorityElement` returns the candidate; on inputs
-- that have a strict majority (the LeetCode guarantee) it IS that element.
--
-- Below the runnable, refl-tested fold we port the count-invariant proof
-- from solution.lean:  `Good` is the loop invariant, established by
-- reverse/snoc induction (`snocRec`, `bm-good`), and it yields
--   • bm-finds-majority : 2 * count m xs > length xs → result ≡ just m
--   • majority-unique    : at most one strict majority
--   • majority-iff       : the candidate is a majority ⇔ one exists.
-- Everything is total (no TERMINATING / pragmas) and passes `agda --safe`.
------------------------------------------------------------------------

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; _<_; _>_; z≤n; s≤s)
open import Data.Nat.Properties
  using ( _≟_; +-identityʳ; +-suc; +-comm; +-monoˡ-≤; +-monoʳ-≤; +-mono-≤
        ; *-suc; *-distribˡ-+; *-mono-≤; n≤1+n; m≤m+n; ≤-refl; ≤-trans; <-irrefl)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Product using (_×_; _,_; proj₁; ∃-syntax)
open import Data.List using (List; []; _∷_; foldl; _++_; length; reverse)
open import Data.List.Properties using (unfold-reverse; reverse-involutive; foldl-++)
open import Relation.Nullary using (¬_; yes; no)
open import Relation.Binary.PropositionalEquality
  using (_≡_; _≢_; refl; sym; trans; cong; subst; subst₂)
open import Data.Empty using (⊥; ⊥-elim)

------------------------------------------------------------------------
-- The runnable fold (must-have)
------------------------------------------------------------------------

step : Maybe ℕ × ℕ → ℕ → Maybe ℕ × ℕ
step (_       , zero)  x = just x , 1
step (nothing , suc k) x = just x , 1
step (just c  , suc k) x with x ≟ c
... | yes _ = just c , suc (suc k)
... | no  _ = just c , k

bm : List ℕ → Maybe ℕ × ℕ
bm xs = foldl step (nothing , 0) xs

majorityElement : List ℕ → Maybe ℕ
majorityElement xs = proj₁ (bm xs)

-- compile-time tests
_ : majorityElement (3 ∷ 2 ∷ 3 ∷ []) ≡ just 3
_ = refl

_ : majorityElement (2 ∷ 2 ∷ 1 ∷ 1 ∷ 1 ∷ 2 ∷ 2 ∷ []) ≡ just 2
_ = refl

_ : majorityElement [] ≡ nothing
_ = refl

------------------------------------------------------------------------
-- Occurrence count and its basic lemmas
------------------------------------------------------------------------

count : ℕ → List ℕ → ℕ
count x [] = 0
count x (y ∷ ys) with x ≟ y
... | yes _ = suc (count x ys)
... | no  _ = count x ys

count-here : ∀ x ys → count x (x ∷ ys) ≡ suc (count x ys)
count-here x ys with x ≟ x
... | yes _   = refl
... | no  x≢x = ⊥-elim (x≢x refl)

count-there : ∀ x z ys → x ≢ z → count x (z ∷ ys) ≡ count x ys
count-there x z ys x≢z with x ≟ z
... | yes x≡z = ⊥-elim (x≢z x≡z)
... | no  _   = refl

count-snoc-self : ∀ x xs → count x (xs ++ x ∷ []) ≡ suc (count x xs)
count-snoc-self x [] with x ≟ x
... | yes _   = refl
... | no  x≢x = ⊥-elim (x≢x refl)
count-snoc-self x (y ∷ ys) with x ≟ y
... | yes _ = cong suc (count-snoc-self x ys)
... | no  _ = count-snoc-self x ys

count-snoc-self′ : ∀ v y xs → v ≡ y → count v (xs ++ y ∷ []) ≡ suc (count v xs)
count-snoc-self′ v y xs refl = count-snoc-self v xs

count-snoc-of-ne : ∀ a x xs → a ≢ x → count a (xs ++ x ∷ []) ≡ count a xs
count-snoc-of-ne a x [] a≢x with a ≟ x
... | yes a≡x = ⊥-elim (a≢x a≡x)
... | no  _   = refl
count-snoc-of-ne a x (y ∷ ys) a≢x with a ≟ y
... | yes _ = cong suc (count-snoc-of-ne a x ys a≢x)
... | no  _ = count-snoc-of-ne a x ys a≢x

length-snoc : ∀ (xs : List ℕ) x → length (xs ++ x ∷ []) ≡ suc (length xs)
length-snoc []       x = refl
length-snoc (y ∷ ys) x = cong suc (length-snoc ys x)

count-two-le : ∀ a b xs → a ≢ b → count a xs + count b xs ≤ length xs
count-two-le a b []       a≢b = z≤n
count-two-le a b (x ∷ xs) a≢b with a ≟ x | b ≟ x
... | yes a≡x | yes b≡x = ⊥-elim (a≢b (trans a≡x (sym b≡x)))
... | yes _   | no  _   = s≤s (count-two-le a b xs a≢b)
... | no  _   | yes _   =
      subst (_≤ suc (length xs)) (sym (+-suc (count a xs) (count b xs)))
            (s≤s (count-two-le a b xs a≢b))
... | no  _   | no  _   = ≤-trans (count-two-le a b xs a≢b) (n≤1+n (length xs))

------------------------------------------------------------------------
-- Small arithmetic helpers (replace Lean's `omega`)
------------------------------------------------------------------------

≤-cong : ∀ {a a′ b b′} → a ≡ a′ → b ≡ b′ → a′ ≤ b′ → a ≤ b
≤-cong p q r = subst₂ _≤_ (sym p) (sym q) r

+1≡suc : ∀ a → a + 1 ≡ suc a
+1≡suc a = +-comm a 1

E1 : ∀ a → 2 * suc a ≡ suc (suc (2 * a))
E1 a = *-suc 2 a

+1≤suc : ∀ {a b} → a ≤ b → a + 1 ≤ suc b
+1≤suc {a} {b} h = subst (_≤ suc b) (sym (+1≡suc a)) (s≤s h)

2suc≤suc+1 : ∀ {a b} → 2 * a ≤ b → 2 * suc a ≤ suc b + 1
2suc≤suc+1 {a} {b} h = ≤-cong (E1 a) (+1≡suc (suc b)) (s≤s (s≤s h))

+suc≤suc : ∀ {a j b} → a + suc j ≤ b → a + suc (suc j) ≤ suc b
+suc≤suc {a} {j} {b} h = subst (_≤ suc b) (sym (+-suc a (suc j))) (s≤s h)

2suc≤ : ∀ {a b j} → 2 * a ≤ b + suc j → 2 * suc a ≤ suc b + suc (suc j)
2suc≤ {a} {b} {j} h = ≤-cong (E1 a) (cong suc (+-suc b (suc j))) (s≤s (s≤s h))

------------------------------------------------------------------------
-- Reverse / snoc induction principle
------------------------------------------------------------------------

snocRec : {P : List ℕ → Set}
        → P []
        → (∀ ys y → P ys → P (ys ++ y ∷ []))
        → ∀ xs → P xs
snocRec {P} hnil hsnoc xs = subst P (reverse-involutive xs) (key (reverse xs))
  where
    key : ∀ rs → P (reverse rs)
    key []       = hnil
    key (r ∷ rs) =
      subst P (sym (unfold-reverse r rs)) (hsnoc (reverse rs) r (key rs))

------------------------------------------------------------------------
-- The Boyer–Moore invariant
------------------------------------------------------------------------

Good : List ℕ → Maybe ℕ × ℕ → Set
Good xs (nothing , _) = xs ≡ []
Good xs (just c , k)  =
    (∀ v → v ≢ c → 2 * count v xs + k ≤ length xs)
  × (2 * count c xs ≤ length xs + k)

goodSingleton : ∀ y → Good (y ∷ []) (just y , 1)
goodSingleton y = h1 , h2
  where
    h1 : ∀ v → v ≢ y → 2 * count v (y ∷ []) + 1 ≤ length (y ∷ [])
    h1 v v≢y rewrite count-there v y [] v≢y = ≤-refl
    h2 : 2 * count y (y ∷ []) ≤ length (y ∷ []) + 1
    h2 rewrite count-here y [] = ≤-refl

goodStep : ∀ ys y st → Good ys st → Good (ys ++ y ∷ []) (step st y)
goodStep ys y (nothing , zero)  g rewrite g = goodSingleton y
goodStep ys y (nothing , suc k) g rewrite g = goodSingleton y
goodStep ys y (just c , zero) (H1 , H2) = nh1 , nh2
  where
    bz : ∀ z → 2 * count z ys ≤ length ys
    bz z with z ≟ c
    ... | yes z≡c = subst (λ w → 2 * count w ys ≤ length ys) (sym z≡c)
                      (subst (2 * count c ys ≤_) (+-identityʳ (length ys)) H2)
    ... | no  z≢c = subst (_≤ length ys) (+-identityʳ (2 * count z ys)) (H1 z z≢c)
    nh1 : ∀ v → v ≢ y → 2 * count v (ys ++ y ∷ []) + 1 ≤ length (ys ++ y ∷ [])
    nh1 v v≢y rewrite count-snoc-of-ne v y ys v≢y | length-snoc ys y = +1≤suc (bz v)
    nh2 : 2 * count y (ys ++ y ∷ []) ≤ length (ys ++ y ∷ []) + 1
    nh2 rewrite count-snoc-self y ys | length-snoc ys y = 2suc≤suc+1 (bz y)
goodStep ys y (just c , suc k) (H1 , H2) with y ≟ c
... | yes y≡c = nh1 , nh2
  where
    nh1 : ∀ v → v ≢ c → 2 * count v (ys ++ y ∷ []) + suc (suc k) ≤ length (ys ++ y ∷ [])
    nh1 v v≢c rewrite count-snoc-of-ne v y ys (λ v≡y → v≢c (trans v≡y y≡c))
                    | length-snoc ys y = +suc≤suc (H1 v v≢c)
    nh2 : 2 * count c (ys ++ y ∷ []) ≤ length (ys ++ y ∷ []) + suc (suc k)
    nh2 rewrite y≡c | count-snoc-self c ys | length-snoc ys c = 2suc≤ H2
... | no  y≢c = nh1 , nh2
  where
    nh1 : ∀ v → v ≢ c → 2 * count v (ys ++ y ∷ []) + k ≤ length (ys ++ y ∷ [])
    nh1 v v≢c with v ≟ y
    ... | yes v≡y rewrite count-snoc-self′ v y ys v≡y | length-snoc ys y =
          ≤-cong (cong (_+ k) (E1 (count v ys))) refl
            (s≤s (subst (_≤ length ys) (+-suc (2 * count v ys) k) (H1 v v≢c)))
    ... | no  v≢y rewrite count-snoc-of-ne v y ys v≢y | length-snoc ys y =
          ≤-trans (≤-trans (+-monoʳ-≤ (2 * count v ys) (n≤1+n k)) (H1 v v≢c))
                  (n≤1+n (length ys))
    nh2 : 2 * count c (ys ++ y ∷ []) ≤ length (ys ++ y ∷ []) + k
    nh2 rewrite count-snoc-of-ne c y ys (λ c≡y → y≢c (sym c≡y)) | length-snoc ys y =
          subst (2 * count c ys ≤_) (+-suc (length ys) k) H2

bm-snoc : ∀ xs x → bm (xs ++ x ∷ []) ≡ step (bm xs) x
bm-snoc xs x = foldl-++ step (nothing , 0) xs (x ∷ [])

bm-good : ∀ xs → Good xs (bm xs)
bm-good = snocRec base ind
  where
    base : Good [] (bm [])
    base = refl
    ind : ∀ ys y → Good ys (bm ys) → Good (ys ++ y ∷ []) (bm (ys ++ y ∷ []))
    ind ys y ih = subst (Good (ys ++ y ∷ [])) (sym (bm-snoc ys y))
                        (goodStep ys y (bm ys) ih)

------------------------------------------------------------------------
-- Downstream corollaries
------------------------------------------------------------------------

-- (1) If ANY strict majority exists, Boyer–Moore's candidate IS it.
bm-finds-majority : ∀ xs m → 2 * count m xs > length xs → majorityElement xs ≡ just m
bm-finds-majority xs m hm with bm xs | bm-good xs
... | nothing , k | g =
      ⊥-elim (lem (subst (λ l → length l < 2 * count m l) g hm))
  where
    lem : 0 < 0 → ⊥
    lem ()
... | just c , k | (H1 , H2) = goal
  where
    goal : just c ≡ just m
    goal with m ≟ c
    ... | yes m≡c = cong just (sym m≡c)
    ... | no  m≢c = ⊥-elim
          (<-irrefl refl
            (≤-trans hm (≤-trans (m≤m+n (2 * count m xs) k) (H1 m m≢c))))

-- (2) At most one strict majority exists.
majority-unique : ∀ xs a b → 2 * count a xs > length xs → 2 * count b xs > length xs → a ≡ b
majority-unique xs a b ha hb with a ≟ b
... | yes a≡b = a≡b
... | no  a≢b = ⊥-elim helper
  where
    ct : count a xs + count b xs ≤ length xs
    ct = count-two-le a b xs a≢b
    step1 : 2 * count a xs + 2 * count b xs ≤ 2 * length xs
    step1 = subst (_≤ 2 * length xs)
              (*-distribˡ-+ 2 (count a xs) (count b xs))
              (*-mono-≤ (≤-refl {x = 2}) ct)
    step3 : suc (length xs) + suc (length xs) ≤ 2 * length xs
    step3 = ≤-trans (+-mono-≤ ha hb) step1
    eqA : suc (length xs) + suc (length xs) ≡ suc (suc (length xs + length xs))
    eqA = cong suc (+-suc (length xs) (length xs))
    eqB : 2 * length xs ≡ length xs + length xs
    eqB = cong (length xs +_) (+-identityʳ (length xs))
    step4 : suc (suc (length xs + length xs)) ≤ length xs + length xs
    step4 = subst₂ _≤_ eqA eqB step3
    helper : ⊥
    helper = <-irrefl refl (≤-trans (n≤1+n (suc (length xs + length xs))) step4)

-- (3) The candidate is a strict majority ⇔ some value is.
majority-iff : ∀ xs →
    (∃[ c ] (majorityElement xs ≡ just c × 2 * count c xs > length xs)
       → ∃[ m ] 2 * count m xs > length xs)
  × (∃[ m ] 2 * count m xs > length xs
       → ∃[ c ] (majorityElement xs ≡ just c × 2 * count c xs > length xs))
majority-iff xs =
    (λ { (c , _ , hc) → c , hc })
  , (λ { (m , hm) → m , bm-finds-majority xs m hm , hm })
