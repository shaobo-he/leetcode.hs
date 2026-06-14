module solution where

-- LeetCode 340: Longest Substring with At Most K Distinct Characters.
-- Sliding window holding at most k distinct chars, with per-char counts;
-- shrink from the left whenever the window has more than k distinct chars.

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _⊔_; _≤_; z≤n; s≤s)
open import Data.Nat.Base using (_<ᵇ_)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; ≤-total; m≤m⊔n; m≤n⊔m; ⊔-lub; m≤n⇒m≤1+n;
         m≤n⇒m⊔n≡n; m≥n⇒m⊔n≡m)
open import Data.Char using (Char; _==_)
open import Data.Char.Properties using (_≟_)
open import Data.List using (List; []; _∷_; length; _++_)
open import Data.Product using (_×_; _,_; Σ; Σ-syntax; ∃; ∃-syntax; proj₁; proj₂)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.String using (String; toList)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; subst)
open import Data.List.Properties
  using (++-assoc; ++-identityʳ; length-++; ∷-injectiveˡ; ∷-injectiveʳ)

-- per-char occurrence counts as an assoc list
incr : Char → List (Char × ℕ) → List (Char × ℕ)
incr x []              = (x , 1) ∷ []
incr x ((y , n) ∷ rest) =
  if x == y then (y , suc n) ∷ rest else (y , n) ∷ incr x rest

decr : Char → List (Char × ℕ) → List (Char × ℕ)
decr x []              = []
decr x ((y , n) ∷ rest) =
  if x == y
    then dropOne n
    else (y , n) ∷ decr x rest
  where
    dropOne : ℕ → List (Char × ℕ)
    dropOne (suc (suc k)) = (y , suc k) ∷ rest
    dropOne _             = rest

-- shrink from the left while the window holds more than k distinct chars.
-- structural on `win`: the recursive call drops the head `d`.
shrinkW : ℕ → List Char → List (Char × ℕ) → (List Char × List (Char × ℕ))
shrinkW k []       counts = ([] , counts)
shrinkW k (d ∷ ds) counts =
  if k <ᵇ length counts
    then shrinkW k ds (decr d counts)
    else (d ∷ ds , counts)

goW : ℕ → List Char → List (Char × ℕ) → ℕ → List Char → ℕ
goW _ _   _      best []       = best
goW k win counts best (c ∷ cs) =
  let r = shrinkW k (win ++ (c ∷ [])) (incr c counts)
  in step r
  where
    step : (List Char × List (Char × ℕ)) → ℕ
    step (win2 , counts2) = goW k win2 counts2 (best ⊔ length win2) cs

lenKDistinct : String → ℕ → ℕ
lenKDistinct s k = goW k [] [] 0 (toList s)

-- compile-time tests (same examples the other languages test)
_ : lenKDistinct "eceba" 2 ≡ 3
_ = refl

_ : lenKDistinct "aa" 1 ≡ 2
_ = refl

_ : lenKDistinct "abee" 1 ≡ 2
_ = refl

------------------------------------------------------------------------
-- ════════════════════════════════════════════════════════════════════
-- FULLY VERIFIED VARIANT.
--
-- The window above (counts-based) is the fast runnable solution.  Below
-- is a structurally-shrinking variant `lenKDistinctV`, for which we prove
-- FULL OPTIMALITY: its answer is *exactly* the maximum length over all
-- substrings with ≤ k distinct characters.
--
--   * lower bound (optimality): every valid substring is no longer than
--     `lenKDistinctV`,
--   * upper bound (realizability): `lenKDistinctV` is the length of some
--     actual valid substring.
--
-- This is a direct port of the Lean `lenKDistinctV_optimal`.  No
-- postulates, holes, TERMINATING/NON_TERMINATING pragmas, primTrustMe,
-- type-in-type, or termination-check disabling are used in this section:
-- the verified `shrinkV`/`goV` are genuinely structural.
-- ════════════════════════════════════════════════════════════════════

-- distinct-character count, via *decidable propositional* Char equality
-- (so we can reason about it; `_==_`/`nub` "distinct" does not reflect ≡)
decElem : Char → List Char → Bool
decElem x []       = false
decElem x (y ∷ ys) with x ≟ y
... | yes _ = true
... | no  _ = decElem x ys

nd : List Char → ℕ
nd []       = zero
nd (x ∷ xs) = if decElem x xs then nd xs else suc (nd xs)

-- membership-after-append: `x` occurs in `xs ++ [c]` iff it occurs in `xs`
-- or equals `c`.  We phrase this as the two implications we actually need.
--
-- (1) if `x` is already in `xs`, it is still in `xs ++ [c]`
decElem-snoc-mono : (x c : Char) (xs : List Char) →
                    decElem x xs ≡ true → decElem x (xs ++ (c ∷ [])) ≡ true
decElem-snoc-mono x c (y ∷ ys) h with x ≟ y
... | yes _ = refl
... | no  _ = decElem-snoc-mono x c ys h

-- (2) appending `c` when `x ≢ c` does not create a new occurrence of `x`
decElem-snoc-neq : (x c : Char) (xs : List Char) → ¬ (x ≡ c) →
                   decElem x (xs ++ (c ∷ [])) ≡ decElem x xs
decElem-snoc-neq x c []       x≢c with x ≟ c
... | yes p = ⊥-elim (x≢c p)
... | no  _ = refl
decElem-snoc-neq x c (y ∷ ys) x≢c with x ≟ y
... | yes _ = refl
... | no  _ = decElem-snoc-neq x c ys x≢c

-- swap helper: decidable equality is symmetric up to ≡
≟-sym : (a b : Char) → ¬ (a ≡ b) → ¬ (b ≡ a)
≟-sym a b a≢b refl = a≢b refl

-- appending `x` always makes `x` occur
decElem-snoc-self : (x : Char) (xs : List Char) →
                    decElem x (xs ++ (x ∷ [])) ≡ true
decElem-snoc-self x []       with x ≟ x
... | yes _   = refl
... | no  x≢x = ⊥-elim (x≢x refl)
decElem-snoc-self x (y ∷ ys) with x ≟ y
... | yes _ = refl
... | no  _ = decElem-snoc-self x ys

-- the boolean head-check `decElem c (x ∷ xs)` when c ≢ x reduces to tail
decElem-cons-neq : (c x : Char) (xs : List Char) → ¬ (c ≡ x) →
                   decElem c (x ∷ xs) ≡ decElem c xs
decElem-cons-neq c x xs c≢x with c ≟ x
... | yes p = ⊥-elim (c≢x p)
... | no  _ = refl

-- a char always occurs at the head of its own cons
decElem-cons-self : (c : Char) (xs : List Char) → decElem c (c ∷ xs) ≡ true
decElem-cons-self c xs with c ≟ c
... | yes _   = refl
... | no  c≢c = ⊥-elim (c≢c refl)

-- appending one char changes the distinct count by exactly 0 or 1.
-- nd (xs ++ [c]) = if c already occurs in xs then nd xs else suc (nd xs)
nd-snoc : (c : Char) (xs : List Char) →
          nd (xs ++ (c ∷ [])) ≡ (if decElem c xs then nd xs else suc (nd xs))
nd-snoc c []       = refl
nd-snoc c (x ∷ xs) with x ≟ c
-- Case x ≡ c : appending c=x makes x present; LHS = nd (xs ++ [x]) and
-- decElem c (x ∷ xs) = true (since c = x), so RHS = nd (x ∷ xs).
nd-snoc c (x ∷ xs) | yes refl
  rewrite decElem-snoc-self x xs | decElem-cons-self x xs = nd-snoc x xs
-- Case x ≢ c : decElem x (xs++[c]) = decElem x xs, recurse via IH.
nd-snoc c (x ∷ xs) | no x≢c
  rewrite decElem-snoc-neq x c xs x≢c
        | decElem-cons-neq c x xs (≟-sym x c x≢c)
        | nd-snoc c xs
  with decElem x xs | decElem c xs
... | true  | true  = refl
... | true  | false = refl
... | false | true  = refl
... | false | false = refl

-- … hence the distinct count is monotone under append.
nd-mono : (xs : List Char) (c : Char) → nd xs ≤ nd (xs ++ (c ∷ []))
nd-mono xs c rewrite nd-snoc c xs with decElem c xs
... | true  = ≤-refl
... | false = m≤n⇒m≤1+n ≤-refl

------------------------------------------------------------------------
-- Our own Suffix / Infix relations (as in the Idris blueprint): simpler
-- to reduce than the Pointwise-based stdlib ones, and genuinely the real
-- contiguous-substring / suffix relations.
------------------------------------------------------------------------

-- sf is a suffix of w
data _Suffix_ {A : Set} : List A → List A → Set where
  here  : ∀ {w} → w Suffix w
  there : ∀ {sf w x} → sf Suffix w → sf Suffix (x ∷ w)

-- sub is a contiguous substring (infix) of full: ∃ before after, before ++ sub ++ after ≡ full
record _Infix_ {A : Set} (sub full : List A) : Set where
  constructor infix-mk
  field
    before : List A
    after  : List A
    eq     : before ++ (sub ++ after) ≡ full

-- structural Boolean ≤, so `validK k []` and `shrinkV` reduce definitionally
leNat : ℕ → ℕ → Bool
leNat zero    _       = true
leNat (suc _) zero    = false
leNat (suc a) (suc b) = leNat a b

leNat⇒≤ : (a b : ℕ) → leNat a b ≡ true → a ≤ b
leNat⇒≤ zero    b       _ = z≤n
leNat⇒≤ (suc a) (suc b) p = s≤s (leNat⇒≤ a b p)

≤⇒leNat : {a b : ℕ} → a ≤ b → leNat a b ≡ true
≤⇒leNat z≤n       = refl
≤⇒leNat (s≤s p)   = ≤⇒leNat p

-- if leNat a b is false then b < a, i.e. ¬ (a ≤ b)
leNat-false⇒¬≤ : (a b : ℕ) → leNat a b ≡ false → ¬ (a ≤ b)
leNat-false⇒¬≤ (suc a) zero    _ ()
leNat-false⇒¬≤ (suc a) (suc b) p (s≤s q) = leNat-false⇒¬≤ a b p q

validK : ℕ → List Char → Bool
validK k cs = leNat (nd cs) k

------------------------------------------------------------------------
-- The VERIFIED structural sliding window.  `shrinkV` drops chars from the
-- FRONT until the distinct count is ≤ k; it is the longest valid (≤ k
-- distinct) suffix of the window.  Both `shrinkV` and `goV` are genuinely
-- structural (recursion strictly on the list argument).
------------------------------------------------------------------------

shrinkV : ℕ → List Char → List Char
shrinkV k []       = []
shrinkV k (x ∷ xs) =
  if leNat (nd (x ∷ xs)) k then x ∷ xs else shrinkV k xs

goV : ℕ → ℕ → List Char → List Char → ℕ
goV k best win []       = best
goV k best win (c ∷ cs) =
  goV k (best ⊔ length (shrinkV k (win ++ (c ∷ [])))) (shrinkV k (win ++ (c ∷ []))) cs

lenKDistinctV : String → ℕ → ℕ
lenKDistinctV s k = goV k 0 [] (toList s)

-- the verified variant agrees with the fast one on the examples
_ : lenKDistinctV "eceba" 2 ≡ 3
_ = refl

_ : lenKDistinctV "aa" 1 ≡ 2
_ = refl

_ : lenKDistinctV "abee" 1 ≡ 2
_ = refl

------------------------------------------------------------------------
-- Suffix / list helper lemmas.
------------------------------------------------------------------------

Suffix-trans : {A : Set} {a b c : List A} → a Suffix b → b Suffix c → a Suffix c
Suffix-trans p here      = p
Suffix-trans p (there q) = there (Suffix-trans p q)

nil-Suffix : {A : Set} (w : List A) → ([] {A = A}) Suffix w
nil-Suffix []       = here
nil-Suffix (x ∷ xs) = there (nil-Suffix xs)

Suffix-length : {A : Set} {sf w : List A} → sf Suffix w → length sf ≤ length w
Suffix-length here      = ≤-refl
Suffix-length (there p) = m≤n⇒m≤1+n (Suffix-length p)

-- the only suffix of [] is []
Suffix-nil⇒nil : {A : Set} {sf : List A} → sf Suffix [] → sf ≡ []
Suffix-nil⇒nil here = refl

-- appending the same char preserves suffix-hood
suffix-snoc-mono : {sf w : List Char} (c : Char) → sf Suffix w →
                   (sf ++ (c ∷ [])) Suffix (w ++ (c ∷ []))
suffix-snoc-mono c here      = here
suffix-snoc-mono c (there p) = there (suffix-snoc-mono c p)

-- decompose a suffix of `xs ++ [c]`
suffix-snoc-decomp : {sf xs : List Char} {c : Char} →
                     sf Suffix (xs ++ (c ∷ [])) →
                     (sf ≡ []) ⊎ (Σ[ sf0 ∈ List Char ] (sf ≡ sf0 ++ (c ∷ []) × sf0 Suffix xs))
-- xs = [] : the list is (c ∷ []); sf is either (c ∷ []) or []
suffix-snoc-decomp {sf} {[]}      {c} here              = inj₂ ([] , refl , here)
suffix-snoc-decomp {sf} {[]}      {c} (there p) with Suffix-nil⇒nil p
... | refl = inj₁ refl
suffix-snoc-decomp {sf} {x ∷ xs}  {c} here      = inj₂ (x ∷ xs , refl , here)
suffix-snoc-decomp {sf} {x ∷ xs}  {c} (there p) with suffix-snoc-decomp {xs = xs} p
... | inj₁ refl              = inj₁ refl
... | inj₂ (sf0 , refl , q)  = inj₂ (sf0 , refl , there q)

------------------------------------------------------------------------
-- shrinkV lemmas.
------------------------------------------------------------------------

-- shrinkV only drops from the front, so the window stays a suffix
shrinkV-suffix : (k : ℕ) (w : List Char) → shrinkV k w Suffix w
shrinkV-suffix k []       = here
shrinkV-suffix k (x ∷ xs) with leNat (nd (x ∷ xs)) k
... | true  = here
... | false = there (shrinkV-suffix k xs)

-- and the returned window always has ≤ k distinct chars
shrinkV-valid : (k : ℕ) (w : List Char) → nd (shrinkV k w) ≤ k
shrinkV-valid k []       = z≤n
shrinkV-valid k (x ∷ xs) with leNat (nd (x ∷ xs)) k | leNat⇒≤ (nd (x ∷ xs)) k
... | true  | p = p refl
... | false | _ = shrinkV-valid k xs

-- the core local lemma: every valid suffix of w is a suffix of shrinkV k w
shrinkV-longest : (k : ℕ) (sf : List Char) → nd sf ≤ k →
                  (w : List Char) → sf Suffix w → sf Suffix shrinkV k w
shrinkV-longest k sf hv []       p with Suffix-nil⇒nil p
... | refl = here
shrinkV-longest k sf hv (x ∷ xs) p with leNat (nd (x ∷ xs)) k | leNat-false⇒¬≤ (nd (x ∷ xs)) k
... | true  | _      = p
... | false | hgt with p
...   | here      = ⊥-elim (hgt refl hv)
...   | there p'  = shrinkV-longest k sf hv xs p'

------------------------------------------------------------------------
-- The window invariant: `win` is the LONGEST valid suffix of the consumed
-- prefix `P`.  Threaded through `goV` it yields the lower bound.
------------------------------------------------------------------------

record IsLVS (k : ℕ) (win P : List Char) : Set where
  constructor lvs-mk
  field
    win-suf   : win Suffix P
    win-valid : nd win ≤ k
    win-all   : (sf : List Char) → sf Suffix P → nd sf ≤ k → sf Suffix win
open IsLVS

lvs-step : (k : ℕ) (win P : List Char) (c : Char) → IsLVS k win P →
           IsLVS k (shrinkV k (win ++ (c ∷ []))) (P ++ (c ∷ []))
lvs-step k win P c h = lvs-mk sufP valP allP
  where
    sufP : shrinkV k (win ++ (c ∷ [])) Suffix (P ++ (c ∷ []))
    sufP = Suffix-trans (shrinkV-suffix k (win ++ (c ∷ [])))
                        (suffix-snoc-mono c (win-suf h))

    valP : nd (shrinkV k (win ++ (c ∷ []))) ≤ k
    valP = shrinkV-valid k (win ++ (c ∷ []))

    allP : (sf : List Char) → sf Suffix (P ++ (c ∷ [])) → nd sf ≤ k →
           sf Suffix shrinkV k (win ++ (c ∷ []))
    allP sf hsf hv with suffix-snoc-decomp {xs = P} hsf
    ... | inj₁ refl = nil-Suffix _
    ... | inj₂ (sf0 , refl , hsf0) =
      -- sf = sf0 ++ [c]; sf0 is a valid suffix of P (by nd-mono), so it is a
      -- suffix of win, hence sf0 ++ [c] is a suffix of win ++ [c], hence of
      -- shrinkV k (win ++ [c]) by shrinkV-longest.
      shrinkV-longest k (sf0 ++ (c ∷ [])) hv (win ++ (c ∷ []))
        (suffix-snoc-mono c (win-all h sf0 hsf0 h0v))
      where
        h0v : nd sf0 ≤ k
        h0v = ≤-trans (nd-mono sf0 c) hv

------------------------------------------------------------------------
-- LOWER BOUND (optimality): the window's answer ≥ length of every valid
-- substring.  This is the part the Idris file left open.
------------------------------------------------------------------------

goV-mono : (k : ℕ) (rem : List Char) (best : ℕ) (win : List Char) →
           best ≤ goV k best win rem
goV-mono k []       best win = ≤-refl
goV-mono k (c ∷ cs) best win =
  ≤-trans (m≤m⊔n best (length (shrinkV k (win ++ (c ∷ [])))))
          (goV-mono k cs (best ⊔ length (shrinkV k (win ++ (c ∷ [])))) (shrinkV k (win ++ (c ∷ []))))

goVOpt : (k : ℕ) (rem win : List Char) (best : ℕ) (P : List Char) →
         IsLVS k win P → length win ≤ best →
         (sub pre suf : List Char) → rem ≡ pre ++ suf →
         sub Suffix (P ++ pre) → nd sub ≤ k →
         length sub ≤ goV k best win rem
goVOpt k [] win best P hinv hwb sub pre suf hrem hsub hsv
  -- rem = [] ⇒ pre = [], so sub Suffix P; use the invariant.
  with pre | suf | hrem
... | [] | [] | refl =
  ≤-trans (Suffix-length (win-all hinv sub
            (subst (sub Suffix_) (++-identityʳ P) hsub) hsv)) hwb
goVOpt k (c ∷ cs) win best P hinv hwb sub []         suf hrem hsub hsv =
  -- pre = [] ⇒ sub Suffix P, bounded by best ≤ goV …
  let sub≤best = ≤-trans (Suffix-length (win-all hinv sub
                   (subst (sub Suffix_) (++-identityʳ P) hsub) hsv)) hwb
  in ≤-trans sub≤best
       (≤-trans (m≤m⊔n best (length (shrinkV k (win ++ (c ∷ [])))))
                (goV-mono k cs _ _))
goVOpt k (c ∷ cs) win best P hinv hwb sub (a ∷ pre') suf hrem hsub hsv
  -- pre = a ∷ pre'.  From rem = (a ∷ pre') ++ suf get a = c, cs = pre' ++ suf.
  with ∷-injectiveˡ hrem | ∷-injectiveʳ hrem
... | refl | hcs =
  goVOpt k cs (shrinkV k (win ++ (c ∷ []))) (best ⊔ length (shrinkV k (win ++ (c ∷ []))))
         (P ++ (c ∷ [])) (lvs-step k win P c hinv)
         (m≤n⊔m best (length (shrinkV k (win ++ (c ∷ [])))))
         sub pre' suf hcs hsub' hsv
  where
    -- sub Suffix (P ++ (c ∷ pre')) ≡ sub Suffix ((P ++ [c]) ++ pre')
    hsub' : sub Suffix ((P ++ (c ∷ [])) ++ pre')
    hsub' = subst (sub Suffix_) (sym (++-assoc P (c ∷ []) pre')) hsub

-- any list is a suffix of anything appended in front of it
suffix-append : {A : Set} (xs sf : List A) → sf Suffix (xs ++ sf)
suffix-append []       sf = here
suffix-append (x ∷ xs) sf = there (suffix-append xs sf)

-- the trivial starting invariant
isLVS-init : (k : ℕ) → IsLVS k [] []
isLVS-init k = lvs-mk here z≤n (λ sf hsf _ → hsf)

-- OPTIMALITY: every valid substring is no longer than the window's answer.
window-optimal : (s : String) (k : ℕ) (sub : List Char) →
                 sub Infix (toList s) → nd sub ≤ k →
                 length sub ≤ lenKDistinctV s k
window-optimal s k sub hsub hv =
  goVOpt k (toList s) [] 0 [] (isLVS-init k) z≤n
         sub (before ++ sub) after
         (sym (trans (++-assoc before sub after) (_Infix_.eq hsub)))
         (suffix-append before sub) hv
  where
    open _Infix_ hsub using (before; after)

------------------------------------------------------------------------
-- UPPER BOUND (realizability): the window's answer is the length of some
-- actual valid substring.
------------------------------------------------------------------------

-- a suffix splits off a prefix
Suffix⇒split : {A : Set} {sf w : List A} → sf Suffix w →
               Σ[ p ∈ List A ] (p ++ sf ≡ w)
Suffix⇒split here      = [] , refl
Suffix⇒split (there {x = x} p) with Suffix⇒split p
... | (pre , refl) = x ∷ pre , refl

-- a suffix of a prefix of `full` is an infix of `full`
suffix-prefix-infix : {a b full : List Char} → a Suffix b →
                      Σ[ t ∈ List Char ] (b ++ t ≡ full) → a Infix full
suffix-prefix-infix {a} {b} {full} hs (t , ht) with Suffix⇒split hs
... | (p , refl) =
  infix-mk p t (trans (sym (++-assoc p a t)) ht)

-- "the current best length is realized by an actual valid substring"
record Realized (k : ℕ) (full : List Char) (b : ℕ) : Set where
  constructor realized-mk
  field
    rsub  : List Char
    rinf  : rsub Infix full
    rval  : nd rsub ≤ k
    rlen  : length rsub ≡ b
open Realized

goVReal : (k : ℕ) (full rem win : List Char) (best : ℕ) (P : List Char) →
          P ++ rem ≡ full → win Suffix P → nd win ≤ k →
          Realized k full best →
          Realized k full (goV k best win rem)
goVReal k full []       win best P hPrem hwP hwv real = real
goVReal k full (c ∷ cs) win best P hPrem hwP hwv real =
  goVReal k full cs (shrinkV k (win ++ (c ∷ [])))
          (best ⊔ length (shrinkV k (win ++ (c ∷ [])))) (P ++ (c ∷ []))
          hP'full hwP' (shrinkV-valid k (win ++ (c ∷ []))) real'
  where
    -- (P ++ [c]) ++ cs ≡ full
    hP'full : (P ++ (c ∷ [])) ++ cs ≡ full
    hP'full = trans (++-assoc P (c ∷ []) cs) hPrem

    hwP' : shrinkV k (win ++ (c ∷ [])) Suffix (P ++ (c ∷ []))
    hwP' = Suffix-trans (shrinkV-suffix k (win ++ (c ∷ [])))
                        (suffix-snoc-mono c hwP)

    hwinf : shrinkV k (win ++ (c ∷ [])) Infix full
    hwinf = suffix-prefix-infix hwP' (cs , hP'full)

    -- the new best (best ⊔ |shrinkV …|) is realized
    real' : Realized k full (best ⊔ length (shrinkV k (win ++ (c ∷ []))))
    real' with ≤-total (length (shrinkV k (win ++ (c ∷ [])))) best
    ... | inj₁ wlen≤best =
      -- max = best, keep the old witness; rewrite its length
      realized-mk (rsub real) (rinf real) (rval real)
                  (trans (rlen real) (sym (m≥n⇒m⊔n≡m wlen≤best)))
    ... | inj₂ best≤wlen =
      -- max = |shrinkV …|, the shrunk window is the witness
      realized-mk (shrinkV k (win ++ (c ∷ []))) hwinf
                  (shrinkV-valid k (win ++ (c ∷ [])))
                  (sym (m≤n⇒m⊔n≡n best≤wlen))

-- [] is an infix of any list
nil-Infix : (full : List Char) → ([] {A = Char}) Infix full
nil-Infix full = infix-mk [] full refl

-- REALIZABILITY: the window's answer is realized by an actual valid substring.
window-realizable : (s : String) (k : ℕ) → Realized k (toList s) (lenKDistinctV s k)
window-realizable s k =
  goVReal k (toList s) (toList s) [] 0 [] refl here z≤n
          (realized-mk [] (nil-Infix (toList s)) z≤n refl)

------------------------------------------------------------------------
-- FULL OPTIMALITY: `lenKDistinctV s k` is EXACTLY the maximum length over
-- substrings with ≤ k distinct chars — both the lower bound (left open in
-- Idris) and the upper bound.
------------------------------------------------------------------------

lenKDistinctV-optimal :
  (s : String) (k : ℕ) →
  -- realizability: there is a valid substring of this exact length …
  (Σ[ sub ∈ List Char ]
     (sub Infix (toList s) × nd sub ≤ k × length sub ≡ lenKDistinctV s k))
  ×
  -- optimality: … and no valid substring is longer.
  ((sub : List Char) → sub Infix (toList s) → nd sub ≤ k →
     length sub ≤ lenKDistinctV s k)
lenKDistinctV-optimal s k =
  (rsub R , rinf R , rval R , rlen R)
  , (λ sub hi hv → window-optimal s k sub hi hv)
  where
    R : Realized k (toList s) (lenKDistinctV s k)
    R = window-realizable s k

