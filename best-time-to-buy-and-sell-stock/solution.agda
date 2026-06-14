module solution where

------------------------------------------------------------------------
-- LeetCode 121: Best Time to Buy and Sell Stock -- single pass tracking the
-- lowest price so far and the best profit, with an Agda proof of CORRECTNESS:
-- `maxProfit prices` is *exactly* the best profit over all valid trades (buy on
-- some day, sell on a later-or-equal day).
--
--   * maxProfit-optimal     -- no trade beats the answer (s ∸ b ≤ maxProfit);
--   * maxProfit-achievable  -- the answer is realised by an actual trade.
--
-- Port of the Lean proof (solution.lean) / Idris 2 proof (solution.idr).  We
-- use ℕ prices with monus (`_∸_` truncates at 0): this is faithful, since a
-- losing or same-day trade has profit 0 and `maxProfit` is max(0, best real
-- gain) -- exactly what monus gives.  Unlike the core-only Idris port we lean
-- on agda-stdlib for the arithmetic, and `Trade` is an indexed inductive so
-- pattern matching recovers the indices directly (no explicit-field workaround).
--
-- The whole file type-checks with `agda` and contains NONE of: postulate,
-- holes (`?`/`{!!}`), `TERMINATING`/`NON_TERMINATING`, `primTrustMe`,
-- `Agda.Builtin.TrustMe`, `--type-in-type`, `--no-termination-check`.  Every
-- proof passes the termination/coverage checks honestly.
------------------------------------------------------------------------

open import Data.Nat using (ℕ; zero; suc; _⊔_; _⊓_; _∸_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; m≤m⊔n; m≤n⊔m; m⊓n≤m; m⊓n≤n; ⊔-sel; ⊓-sel;
         ∸-monoʳ-≤; n∸n≡0; m∸n≤m)
open import Data.List using (List; []; _∷_; _++_)
open import Data.List.Properties using (++-assoc; ++-identityʳ)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (∃; ∃-syntax; _×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; cong; subst)

------------------------------------------------------------------------
-- Single pass: `lowest` = min price so far, `best` = best profit so far (monus).
------------------------------------------------------------------------

go : ℕ → ℕ → List ℕ → ℕ
go lowest best []       = best
go lowest best (x ∷ xs) = go (lowest ⊓ x) (best ⊔ (x ∸ lowest)) xs

maxProfit : List ℕ → ℕ
maxProfit []       = 0
maxProfit (p ∷ ps) = go p 0 ps

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : maxProfit (7 ∷ 1 ∷ 5 ∷ 3 ∷ 6 ∷ 4 ∷ []) ≡ 5
_ = refl

_ : maxProfit (7 ∷ 6 ∷ 4 ∷ 3 ∷ 1 ∷ []) ≡ 0
_ = refl

------------------------------------------------------------------------
-- A tiny structural membership predicate (own `_∈_`, so its eliminators are
-- exactly what the proofs need).
------------------------------------------------------------------------

data _∈_ : ℕ → List ℕ → Set where
  here  : ∀ {x xs}            → x ∈ (x ∷ xs)
  there : ∀ {x y ys} → x ∈ ys → x ∈ (y ∷ ys)

-- membership in a singleton forces equality
∈-singleton : ∀ {p x} → p ∈ (x ∷ []) → p ≡ x
∈-singleton here          = refl
∈-singleton (there ())

-- membership distributes over ++ (the "or" direction we need)
∈-++ : ∀ {p ys} (pre : List ℕ) → p ∈ (pre ++ ys) → (p ∈ pre) ⊎ (p ∈ ys)
∈-++ []        m         = inj₂ m
∈-++ (q ∷ qs)  here      = inj₁ here
∈-++ (q ∷ qs)  (there m) with ∈-++ qs m
... | inj₁ mpre = inj₁ (there mpre)
... | inj₂ mys  = inj₂ mys

∈-++-left : ∀ {p ys} {pre : List ℕ} → p ∈ pre → p ∈ (pre ++ ys)
∈-++-left here      = here
∈-++-left (there m) = there (∈-++-left m)

∈-++-right : ∀ {p ys} (pre : List ℕ) → p ∈ ys → p ∈ (pre ++ ys)
∈-++-right []       m = m
∈-++-right (q ∷ qs) m = there (∈-++-right qs m)

------------------------------------------------------------------------
-- A valid trade in `prices`: buy at `b`, sell at `s` on a later-or-equal day.
--   here  : buy at the head, sell at any element from the head onward;
--   there : the whole trade happens later in the list.
-- Indexed by the list / buy / sell, so matching recovers the indices.
------------------------------------------------------------------------

data Trade : List ℕ → ℕ → ℕ → Set where
  here  : ∀ {b s rest} → s ∈ (b ∷ rest)   → Trade (b ∷ rest) b s
  there : ∀ {x b s rest} → Trade rest b s → Trade (x ∷ rest) b s

-- no trade in the empty list
trade-[]-absurd : ∀ {b s} → Trade [] b s → ⊥
trade-[]-absurd ()

------------------------------------------------------------------------
-- `lowest` is the minimum of the (nonempty) consumed prefix.
------------------------------------------------------------------------

IsMin : ℕ → List ℕ → Set
IsMin lowest pre = (lowest ∈ pre) × (∀ p → p ∈ pre → lowest ≤ p)

------------------------------------------------------------------------
-- A trade in `pre ++ [x]` either lies inside `pre`, or sells at `x` (buying at
-- some earlier element of `pre`, or at `x` itself).  Mirrors Lean `trade_snoc`.
------------------------------------------------------------------------

trade-snoc : ∀ {x b s} (pre : List ℕ) → Trade (pre ++ (x ∷ [])) b s →
             (Trade pre b s) ⊎ ((s ≡ x) × ((b ∈ pre) ⊎ (b ≡ x)))
-- list is [x]; `here` forces b = x, and mem : s ∈ [x] forces s = x.
trade-snoc []        (here mem)  = inj₂ (∈-singleton mem , inj₂ refl)
trade-snoc []        (there t)   = ⊥-elim (trade-[]-absurd t)
-- b is the head p.  mem : s ∈ (p ∷ (pre' ++ [x])).
trade-snoc (p ∷ pre') (here here)        = inj₁ (here here)        -- s = p, inside
trade-snoc (p ∷ pre') (here (there mem)) with ∈-++ pre' mem
... | inj₁ msPre = inj₁ (here (there msPre))
... | inj₂ msX   = inj₂ (∈-singleton msX , inj₁ here)             -- s = x, buy head p
trade-snoc (p ∷ pre') (there t') with trade-snoc pre' t'
... | inj₁ tpre'              = inj₁ (there tpre')
... | inj₂ (eqsx , inj₁ mb)   = inj₂ (eqsx , inj₁ (there mb))
... | inj₂ (eqsx , inj₂ eqbx) = inj₂ (eqsx , inj₂ eqbx)

------------------------------------------------------------------------
-- The new running minimum (lowest ⊓ x) is the minimum of pre ++ [x].
-- Shared by both the optimality and achievability inductions.
------------------------------------------------------------------------

step-min : ∀ {lowest x} (pre : List ℕ) → IsMin lowest pre →
           IsMin (lowest ⊓ x) (pre ++ (x ∷ []))
step-min {lowest} {x} pre (hmem , hle) = memNew , leNew
  where
    memNew : (lowest ⊓ x) ∈ (pre ++ (x ∷ []))
    memNew with ⊓-sel lowest x
    ... | inj₁ eq = subst (_∈ _) (sym eq) (∈-++-left hmem)
    ... | inj₂ eq = subst (_∈ _) (sym eq) (∈-++-right pre here)

    leNew : ∀ p → p ∈ (pre ++ (x ∷ [])) → (lowest ⊓ x) ≤ p
    leNew p mp with ∈-++ pre mp
    ... | inj₁ mpre = ≤-trans (m⊓n≤m lowest x) (hle p mpre)
    ... | inj₂ mx   = subst ((lowest ⊓ x) ≤_) (sym (∈-singleton mx)) (m⊓n≤n lowest x)

------------------------------------------------------------------------
-- OPTIMALITY: no trade beats `go`'s answer.  Generalised over the running
-- state, by induction on the remaining list `xs`.  Mirrors Lean `go_opt`.
------------------------------------------------------------------------

go-opt : ∀ (xs : List ℕ) (pre : List ℕ) (lowest best : ℕ) →
         IsMin lowest pre →
         (∀ b s → Trade pre b s → (s ∸ b) ≤ best) →
         ∀ b s → Trade (pre ++ xs) b s → (s ∸ b) ≤ go lowest best xs
-- pre ++ [] = pre, so the trade is in `pre`; the bound `hub` finishes it.
go-opt []       pre lowest best hmin hub b s ht =
  hub b s (subst (λ l → Trade l b s) (++-identityʳ pre) ht)
go-opt (x ∷ xs) pre lowest best hmin hub b s ht =
  go-opt xs (pre ++ (x ∷ [])) (lowest ⊓ x) (best ⊔ (x ∸ lowest))
         (step-min pre hmin) hub' b s ht'
  where
    hle : ∀ p → p ∈ pre → lowest ≤ p
    hle = proj₂ hmin

    -- any trade in pre ++ [x] has profit ≤ the new best (best ⊔ (x ∸ lowest)).
    hub' : ∀ b' s' → Trade (pre ++ (x ∷ [])) b' s' →
           (s' ∸ b') ≤ (best ⊔ (x ∸ lowest))
    hub' b' s' ht'' with trade-snoc pre ht''
    ... | inj₁ tpre =
            -- profit ≤ best ≤ new best.
            ≤-trans (hub b' s' tpre) (m≤m⊔n best (x ∸ lowest))
    ... | inj₂ (refl , inj₁ mbpre) =
            -- s' = x, b' in pre so lowest ≤ b', hence x ∸ b' ≤ x ∸ lowest ≤ new.
            ≤-trans (∸-monoʳ-≤ x (hle b' mbpre)) (m≤n⊔m best (x ∸ lowest))
    ... | inj₂ (refl , inj₂ refl) =
            -- s' = x = b', so profit x ∸ x = 0 ≤ anything.
            subst (λ d → d ≤ (best ⊔ (x ∸ lowest))) (sym (n∸n≡0 b')) z≤n

    -- re-associate (pre ++ [x]) ++ xs = pre ++ (x ∷ xs).
    ht' : Trade ((pre ++ (x ∷ [])) ++ xs) b s
    ht' = subst (λ l → Trade l b s) (sym (++-assoc pre (x ∷ []) xs)) ht

maxProfit-optimal : ∀ (prices : List ℕ) (b s : ℕ) → Trade prices b s →
                    (s ∸ b) ≤ maxProfit prices
maxProfit-optimal []       b s ht = ⊥-elim (trade-[]-absurd ht)
maxProfit-optimal (p ∷ ps) b s ht = go-opt ps (p ∷ []) p 0 hmin hub b s ht
  where
    hmin : IsMin p (p ∷ [])
    hmin = here , λ q mq → subst (p ≤_) (sym (∈-singleton mq)) ≤-refl
    hub : ∀ b' s' → Trade (p ∷ []) b' s' → (s' ∸ b') ≤ 0
    hub b' s' (here mem)  = subst (λ s'' → (s'' ∸ b') ≤ 0)
                                  (sym (∈-singleton mem))
                                  (subst (_≤ 0) (sym (n∸n≡0 b')) z≤n)
    hub b' s' (there t)   = ⊥-elim (trade-[]-absurd t)

------------------------------------------------------------------------
-- ACHIEVABILITY: the answer is realised by an actual trade.
------------------------------------------------------------------------

-- a trade survives appending more days on the right.  Mirrors Lean `trade_append`.
trade-append : ∀ {pre b s} (ys : List ℕ) → Trade pre b s → Trade (pre ++ ys) b s
trade-append ys (here here)        = here here
trade-append ys (here (there m))   = here (there (∈-++-left m))
trade-append ys (there t)          = there (trade-append ys t)

-- buying at any earlier day `b` and selling at the appended day `x` is a trade.
-- Mirrors Lean `trade_mem_snoc`.
trade-mem-snoc : ∀ {b} (pre : List ℕ) (x : ℕ) → b ∈ pre → Trade (pre ++ (x ∷ [])) b x
trade-mem-snoc (b ∷ rest) x here       = here (there (∈-++-right rest here))
trade-mem-snoc (p ∷ rest) x (there mb) = there (trade-mem-snoc rest x mb)

-- ACHIEVABILITY generalised over the running state.  Mirrors Lean `go_ach`.
go-ach : ∀ (xs : List ℕ) (pre : List ℕ) (lowest best : ℕ) →
         IsMin lowest pre →
         ∃[ b ] ∃[ s ] (Trade pre b s × (s ∸ b ≡ best)) →
         ∃[ b ] ∃[ s ] (Trade (pre ++ xs) b s × (s ∸ b ≡ go lowest best xs))
go-ach []       pre lowest best hmin (b , s , ht , eq) =
  b , s , subst (λ l → Trade l b s) (sym (++-identityʳ pre)) ht , eq
go-ach (x ∷ xs) pre lowest best hmin real =
  let b , s , ht , eq = go-ach xs (pre ++ (x ∷ [])) (lowest ⊓ x) (best ⊔ (x ∸ lowest))
                               (step-min pre hmin) real'
  in b , s , subst (λ l → Trade l b s) (++-assoc pre (x ∷ []) xs) ht , eq
  where
    hmem : lowest ∈ pre
    hmem = proj₁ hmin

    -- a trade in pre ++ [x] realising the new best (best ⊔ (x ∸ lowest)).
    real' : ∃[ b ] ∃[ s ] (Trade (pre ++ (x ∷ [])) b s × (s ∸ b ≡ best ⊔ (x ∸ lowest)))
    real' with ⊔-sel best (x ∸ lowest)
    ... | inj₁ eqmax =
            -- new best is the old best; reuse the existing trade, appended.
            let b , s , ht , eq = real
            in b , s , trade-append (x ∷ []) ht , subst (_≡_ (s ∸ b)) (sym eqmax) eq
    ... | inj₂ eqmax =
            -- new best is x ∸ lowest; buy at lowest (in pre), sell at x.
            lowest , x , trade-mem-snoc pre x hmem , sym eqmax

maxProfit-achievable : ∀ (p : ℕ) (ps : List ℕ) →
  ∃[ b ] ∃[ s ] (Trade (p ∷ ps) b s × (s ∸ b ≡ maxProfit (p ∷ ps)))
maxProfit-achievable p ps = go-ach ps (p ∷ []) p 0 hmin real
  where
    hmin : IsMin p (p ∷ [])
    hmin = here , λ q mq → subst (p ≤_) (sym (∈-singleton mq)) ≤-refl
    real : ∃[ b ] ∃[ s ] (Trade (p ∷ []) b s × (s ∸ b ≡ 0))
    real = p , p , here here , n∸n≡0 p

------------------------------------------------------------------------
-- `maxProfit` is EXACTLY the best achievable trade profit.
------------------------------------------------------------------------

maxProfit-correct : ∀ (p : ℕ) (ps : List ℕ) →
  (∃[ b ] ∃[ s ] (Trade (p ∷ ps) b s × (s ∸ b ≡ maxProfit (p ∷ ps)))) ×
  (∀ b s → Trade (p ∷ ps) b s → (s ∸ b) ≤ maxProfit (p ∷ ps))
maxProfit-correct p ps =
  maxProfit-achievable p ps , λ b s → maxProfit-optimal (p ∷ ps) b s
