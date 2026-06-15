module solution where

-- Longest substring without repeating characters.
-- Sliding window: track the last-seen index per character in an assoc list;
-- when a repeat is seen, jump the window start past its previous position.

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _⊔_; _⊓_; _≤_; _<_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; ≤-antisym; ≤-total; m≤m⊔n; m≤n⊔m; m⊓n≤m; m≤n⇒m≤1+n;
         m≤n⇒m⊔n≡n; m≥n⇒m⊔n≡m; +-comm; +-assoc; m∸n+n≡m; m+n∸n≡m; m+n∸m≡n;
         ∸-+-assoc; m≤n⇒m∸n≡0; m≤m+n; m≤n+m; n≤1+n; suc-injective; 1+n≰n;
         m≤n⇒m⊓n≡m; +-identityʳ; +-suc; <⇒≤; <-trans; +-monoʳ-≤; m+[n∸m]≡n)
open import Data.Char using (Char; _==_; _≟_)
open import Data.List using (List; []; _∷_; length; _++_; drop; take)
open import Data.List.Properties
  using (++-assoc; ++-identityʳ; length-++; ∷-injectiveˡ; ∷-injectiveʳ;
         drop-drop; length-drop; length-take; take++drop≡id; take-drop)
open import Data.Maybe using (Maybe; just; nothing)
open import Function using (case_of_)
open import Data.Product using (_×_; _,_; Σ; Σ-syntax; ∃; ∃-syntax; proj₁; proj₂)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.String using (String; toList)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; cong₂; subst)

-- last-seen index per char as an assoc list
setIdx : Char → ℕ → List (Char × ℕ) → List (Char × ℕ)
setIdx c i []              = (c , i) ∷ []
setIdx c i ((d , j) ∷ rest) =
  if c == d then (c , i) ∷ rest else (d , j) ∷ setIdx c i rest

lookupIdx : Char → List (Char × ℕ) → Maybe ℕ
lookupIdx c []              = nothing
lookupIdx c ((d , j) ∷ rest) = if c == d then just j else lookupIdx c rest

-- Sliding window: jump start past any repeat of the current char.
-- (Lifted to a top-level def `goFast`, as in the Lean blueprint, so the
-- equivalence proof below can recurse on it.  Recursion is genuinely
-- structural on the input list.)
jump : ℕ → Maybe ℕ → ℕ
jump start (just j) = start ⊔ suc j
jump start nothing  = start

goFast : ℕ → ℕ → List (Char × ℕ) → ℕ → List Char → ℕ
goFast _ _     _    best []       = best
goFast i start seen best (c ∷ cs) =
  goFast (suc i) (jump start (lookupIdx c seen)) (setIdx c i seen)
         (best ⊔ (suc i ∸ jump start (lookupIdx c seen))) cs

lengthOfLongest : String → ℕ
lengthOfLongest s = goFast 0 0 [] 0 (toList s)

-- compile-time tests (same examples the other languages test)
_ : lengthOfLongest "abcabcbb" ≡ 3
_ = refl

_ : lengthOfLongest "bbbbb" ≡ 1
_ = refl

_ : lengthOfLongest "pwwkew" ≡ 3
_ = refl

------------------------------------------------------------------------
-- ════════════════════════════════════════════════════════════════════
-- FULLY VERIFIED VARIANT.
--
-- The window above (last-seen-index, O(n)) is the fast runnable solution.
-- Below is a structurally-shrinking variant `lengthOfLongestV`, for which
-- we prove FULL OPTIMALITY: its answer is *exactly* the maximum length
-- over all substrings whose characters are all distinct (no repeats).
--
-- Same machinery as the k-distinct proof, with validity = "all characters
-- distinct" (`Distinct cs := nd cs ≡ length cs`, i.e. the distinct count
-- equals the length) instead of "≤ k distinct".
--
-- No postulates, holes, TERMINATING/NON_TERMINATING pragmas, primTrustMe,
-- type-in-type, or termination-check disabling: `shrinkV`/`goV`/`goFast`
-- are genuinely structural (recursion strictly on the list argument).
-- ════════════════════════════════════════════════════════════════════

-- distinct-character count, via *decidable propositional* Char equality
decElem : Char → List Char → Bool
decElem x []       = false
decElem x (y ∷ ys) with x ≟ y
... | yes _ = true
... | no  _ = decElem x ys

nd : List Char → ℕ
nd []       = zero
nd (x ∷ xs) = if decElem x xs then nd xs else suc (nd xs)

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

≟-sym : (a b : Char) → ¬ (a ≡ b) → ¬ (b ≡ a)
≟-sym a b a≢b refl = a≢b refl

decElem-snoc-self : (x : Char) (xs : List Char) →
                    decElem x (xs ++ (x ∷ [])) ≡ true
decElem-snoc-self x []       with x ≟ x
... | yes _   = refl
... | no  x≢x = ⊥-elim (x≢x refl)
decElem-snoc-self x (y ∷ ys) with x ≟ y
... | yes _ = refl
... | no  _ = decElem-snoc-self x ys

decElem-cons-neq : (c x : Char) (xs : List Char) → ¬ (c ≡ x) →
                   decElem c (x ∷ xs) ≡ decElem c xs
decElem-cons-neq c x xs c≢x with c ≟ x
... | yes p = ⊥-elim (c≢x p)
... | no  _ = refl

decElem-cons-self : (c : Char) (xs : List Char) → decElem c (c ∷ xs) ≡ true
decElem-cons-self c xs with c ≟ c
... | yes _   = refl
... | no  c≢c = ⊥-elim (c≢c refl)

-- appending one char changes the distinct count by exactly 0 or 1.
nd-snoc : (c : Char) (xs : List Char) →
          nd (xs ++ (c ∷ [])) ≡ (if decElem c xs then nd xs else suc (nd xs))
nd-snoc c []       = refl
nd-snoc c (x ∷ xs) with x ≟ c
nd-snoc c (x ∷ xs) | yes refl
  rewrite decElem-snoc-self x xs | decElem-cons-self x xs = nd-snoc x xs
nd-snoc c (x ∷ xs) | no x≢c
  rewrite decElem-snoc-neq x c xs x≢c
        | decElem-cons-neq c x xs (≟-sym x c x≢c)
        | nd-snoc c xs
  with decElem x xs | decElem c xs
... | true  | true  = refl
... | true  | false = refl
... | false | true  = refl
... | false | false = refl

nd-mono : (xs : List Char) (c : Char) → nd xs ≤ nd (xs ++ (c ∷ []))
nd-mono xs c rewrite nd-snoc c xs with decElem c xs
... | true  = ≤-refl
... | false = m≤n⇒m≤1+n ≤-refl

-- the distinct count never exceeds the length
nd-le-length : (cs : List Char) → nd cs ≤ length cs
nd-le-length []       = z≤n
nd-le-length (x ∷ xs) with decElem x xs
... | true  = m≤n⇒m≤1+n (nd-le-length xs)
... | false = s≤s (nd-le-length xs)

-- "all characters distinct": the distinct count equals the length
Distinct : List Char → Set
Distinct cs = nd cs ≡ length cs

distinct-nil : Distinct []
distinct-nil = refl

-- structural Boolean ℕ-equality, so `shrinkV` reduces definitionally
eqNat : ℕ → ℕ → Bool
eqNat zero    zero    = true
eqNat zero    (suc _) = false
eqNat (suc _) zero    = false
eqNat (suc a) (suc b) = eqNat a b

eqNat⇒≡ : (a b : ℕ) → eqNat a b ≡ true → a ≡ b
eqNat⇒≡ zero    zero    _ = refl
eqNat⇒≡ (suc a) (suc b) p = cong suc (eqNat⇒≡ a b p)

≡⇒eqNat : (a : ℕ) → eqNat a a ≡ true
≡⇒eqNat zero    = refl
≡⇒eqNat (suc a) = ≡⇒eqNat a

-- if eqNat a b is false then a ≢ b
eqNat-false⇒≢ : (a b : ℕ) → eqNat a b ≡ false → ¬ (a ≡ b)
eqNat-false⇒≢ (suc a) (suc b) p refl = eqNat-false⇒≢ a b p refl

-- the boolean "is-distinct" test for `shrinkV`, equivalent to `Distinct`
distinctB : List Char → Bool
distinctB cs = eqNat (nd cs) (length cs)

distinctB⇒Distinct : (cs : List Char) → distinctB cs ≡ true → Distinct cs
distinctB⇒Distinct cs p = eqNat⇒≡ (nd cs) (length cs) p

Distinct⇒distinctB : (cs : List Char) → Distinct cs → distinctB cs ≡ true
Distinct⇒distinctB cs h rewrite h = ≡⇒eqNat (length cs)

distinctB-false⇒¬Distinct : (cs : List Char) → distinctB cs ≡ false → ¬ (Distinct cs)
distinctB-false⇒¬Distinct cs p = eqNat-false⇒≢ (nd cs) (length cs) p

-- length grows by one under snoc
length-snoc : (ys : List Char) (d : Char) → length (ys ++ (d ∷ [])) ≡ suc (length ys)
length-snoc ys d rewrite length-++ ys {d ∷ []} = +-comm (length ys) 1

-- a prefix of an all-distinct list is all-distinct.
-- From `Distinct (xs ++ [c])` and `nd xs ≤ length xs`:
--   nd-snoc gives nd(xs++[c]) = (if decElem c xs then nd xs else suc(nd xs)),
--   length-snoc gives length(xs++[c]) = suc(length xs).
-- Combining the two cases shows nd xs ≡ length xs.
distinct-init : (xs : List Char) (c : Char) → Distinct (xs ++ (c ∷ [])) → Distinct xs
distinct-init xs c h with decElem c xs | nd-snoc c xs | nd-le-length xs
-- decElem c xs = true : nd(xs++[c]) = nd xs ; with length(xs++[c]) = suc(length xs)
-- and Distinct we get nd xs ≡ suc(length xs), but nd xs ≤ length xs — impossible.
... | true  | hsnoc | hle =
  ⊥-elim (1+n≰n (subst (_≤ length xs)
           (trans (sym hsnoc) (trans h (length-snoc xs c))) hle))
-- decElem c xs = false : nd(xs++[c]) = suc(nd xs); strip the suc.
... | false | hsnoc | _   =
  suc-injective (trans (sym hsnoc) (trans h (length-snoc xs c)))

------------------------------------------------------------------------
-- Our own Suffix / Infix relations: simpler to reduce than the
-- Pointwise-based stdlib ones, and genuinely the real contiguous-substring
-- / suffix relations.
------------------------------------------------------------------------

-- sf is a suffix of w
data _Suffix_ {A : Set} : List A → List A → Set where
  here  : ∀ {w} → w Suffix w
  there : ∀ {sf w x} → sf Suffix w → sf Suffix (x ∷ w)

-- sub is a contiguous substring (infix) of full
record _Infix_ {A : Set} (sub full : List A) : Set where
  constructor infix-mk
  field
    before : List A
    after  : List A
    eq     : before ++ (sub ++ after) ≡ full

------------------------------------------------------------------------
-- The VERIFIED structural sliding window.  `shrinkV` drops chars from the
-- FRONT until the window is all-distinct; it is the longest all-distinct
-- suffix of the window.  Both `shrinkV` and `goV` are genuinely structural
-- (recursion strictly on the list argument).
------------------------------------------------------------------------

shrinkV : List Char → List Char
shrinkV []       = []
shrinkV (x ∷ xs) =
  if distinctB (x ∷ xs) then x ∷ xs else shrinkV xs

goV : ℕ → List Char → List Char → ℕ
goV best win []       = best
goV best win (c ∷ cs) =
  goV (best ⊔ length (shrinkV (win ++ (c ∷ [])))) (shrinkV (win ++ (c ∷ []))) cs

lengthOfLongestV : String → ℕ
lengthOfLongestV s = goV 0 [] (toList s)

-- the verified variant agrees with the fast one on the examples
_ : lengthOfLongestV "abcabcbb" ≡ 3
_ = refl

_ : lengthOfLongestV "bbbbb" ≡ 1
_ = refl

_ : lengthOfLongestV "pwwkew" ≡ 3
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

Suffix-nil⇒nil : {A : Set} {sf : List A} → sf Suffix [] → sf ≡ []
Suffix-nil⇒nil here = refl

suffix-snoc-mono : {sf w : List Char} (c : Char) → sf Suffix w →
                   (sf ++ (c ∷ [])) Suffix (w ++ (c ∷ []))
suffix-snoc-mono c here      = here
suffix-snoc-mono c (there p) = there (suffix-snoc-mono c p)

suffix-snoc-decomp : {sf xs : List Char} {c : Char} →
                     sf Suffix (xs ++ (c ∷ [])) →
                     (sf ≡ []) ⊎ (Σ[ sf0 ∈ List Char ] (sf ≡ sf0 ++ (c ∷ []) × sf0 Suffix xs))
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
shrinkV-suffix : (w : List Char) → shrinkV w Suffix w
shrinkV-suffix []       = here
shrinkV-suffix (x ∷ xs) with distinctB (x ∷ xs)
... | true  = here
... | false = there (shrinkV-suffix xs)

-- and the returned window is always all-distinct
shrinkV-valid : (w : List Char) → Distinct (shrinkV w)
shrinkV-valid []       = refl
shrinkV-valid (x ∷ xs) with distinctB (x ∷ xs) | distinctB⇒Distinct (x ∷ xs)
... | true  | p = p refl
... | false | _ = shrinkV-valid xs

-- the core local lemma: every all-distinct suffix of w is a suffix of shrinkV w
shrinkV-longest : (sf : List Char) → Distinct sf →
                  (w : List Char) → sf Suffix w → sf Suffix shrinkV w
shrinkV-longest sf hv []       p with Suffix-nil⇒nil p
... | refl = here
shrinkV-longest sf hv (x ∷ xs) p with distinctB (x ∷ xs) | distinctB-false⇒¬Distinct (x ∷ xs)
... | true  | _      = p
... | false | hgt with p
...   | here      = ⊥-elim (hgt refl hv)
...   | there p'  = shrinkV-longest sf hv xs p'

------------------------------------------------------------------------
-- The window invariant: `win` is the LONGEST all-distinct suffix of the
-- consumed prefix `P`.  Threaded through `goV` it yields the lower bound.
------------------------------------------------------------------------

record IsLVS (win P : List Char) : Set where
  constructor lvs-mk
  field
    win-suf   : win Suffix P
    win-valid : Distinct win
    win-all   : (sf : List Char) → sf Suffix P → Distinct sf → sf Suffix win
open IsLVS

lvs-step : (win P : List Char) (c : Char) → IsLVS win P →
           IsLVS (shrinkV (win ++ (c ∷ []))) (P ++ (c ∷ []))
lvs-step win P c h = lvs-mk sufP valP allP
  where
    sufP : shrinkV (win ++ (c ∷ [])) Suffix (P ++ (c ∷ []))
    sufP = Suffix-trans (shrinkV-suffix (win ++ (c ∷ [])))
                        (suffix-snoc-mono c (win-suf h))

    valP : Distinct (shrinkV (win ++ (c ∷ [])))
    valP = shrinkV-valid (win ++ (c ∷ []))

    allP : (sf : List Char) → sf Suffix (P ++ (c ∷ [])) → Distinct sf →
           sf Suffix shrinkV (win ++ (c ∷ []))
    allP sf hsf hv with suffix-snoc-decomp {xs = P} hsf
    ... | inj₁ refl = nil-Suffix _
    ... | inj₂ (sf0 , refl , hsf0) =
      shrinkV-longest (sf0 ++ (c ∷ [])) hv (win ++ (c ∷ []))
        (suffix-snoc-mono c (win-all h sf0 hsf0 h0v))
      where
        h0v : Distinct sf0
        h0v = distinct-init sf0 c hv

------------------------------------------------------------------------
-- LOWER BOUND (optimality): the window's answer ≥ length of every
-- all-distinct substring.
------------------------------------------------------------------------

goV-mono : (rem : List Char) (best : ℕ) (win : List Char) →
           best ≤ goV best win rem
goV-mono []       best win = ≤-refl
goV-mono (c ∷ cs) best win =
  ≤-trans (m≤m⊔n best (length (shrinkV (win ++ (c ∷ [])))))
          (goV-mono cs (best ⊔ length (shrinkV (win ++ (c ∷ [])))) (shrinkV (win ++ (c ∷ []))))

goVOpt : (rem win : List Char) (best : ℕ) (P : List Char) →
         IsLVS win P → length win ≤ best →
         (sub pre suf : List Char) → rem ≡ pre ++ suf →
         sub Suffix (P ++ pre) → Distinct sub →
         length sub ≤ goV best win rem
goVOpt [] win best P hinv hwb sub pre suf hrem hsub hsv
  with pre | suf | hrem
... | [] | [] | refl =
  ≤-trans (Suffix-length (win-all hinv sub
            (subst (sub Suffix_) (++-identityʳ P) hsub) hsv)) hwb
goVOpt (c ∷ cs) win best P hinv hwb sub []         suf hrem hsub hsv =
  let sub≤best = ≤-trans (Suffix-length (win-all hinv sub
                   (subst (sub Suffix_) (++-identityʳ P) hsub) hsv)) hwb
  in ≤-trans sub≤best
       (≤-trans (m≤m⊔n best (length (shrinkV (win ++ (c ∷ [])))))
                (goV-mono cs _ _))
goVOpt (c ∷ cs) win best P hinv hwb sub (a ∷ pre') suf hrem hsub hsv
  with ∷-injectiveˡ hrem | ∷-injectiveʳ hrem
... | refl | hcs =
  goVOpt cs (shrinkV (win ++ (c ∷ []))) (best ⊔ length (shrinkV (win ++ (c ∷ []))))
         (P ++ (c ∷ [])) (lvs-step win P c hinv)
         (m≤n⊔m best (length (shrinkV (win ++ (c ∷ [])))))
         sub pre' suf hcs hsub' hsv
  where
    hsub' : sub Suffix ((P ++ (c ∷ [])) ++ pre')
    hsub' = subst (sub Suffix_) (sym (++-assoc P (c ∷ []) pre')) hsub

-- any list is a suffix of anything appended in front of it
suffix-append : {A : Set} (xs sf : List A) → sf Suffix (xs ++ sf)
suffix-append []       sf = here
suffix-append (x ∷ xs) sf = there (suffix-append xs sf)

-- the trivial starting invariant
isLVS-init : IsLVS [] []
isLVS-init = lvs-mk here distinct-nil (λ sf hsf _ → hsf)

-- OPTIMALITY: every all-distinct substring is no longer than the window's answer.
window-optimal : (s : String) (sub : List Char) →
                 sub Infix (toList s) → Distinct sub →
                 length sub ≤ lengthOfLongestV s
window-optimal s sub hsub hv =
  goVOpt (toList s) [] 0 [] isLVS-init z≤n
         sub (before ++ sub) after
         (sym (trans (++-assoc before sub after) (_Infix_.eq hsub)))
         (suffix-append before sub) hv
  where
    open _Infix_ hsub using (before; after)

------------------------------------------------------------------------
-- UPPER BOUND (realizability): the window's answer is the length of some
-- actual all-distinct substring.
------------------------------------------------------------------------

Suffix⇒split : {A : Set} {sf w : List A} → sf Suffix w →
               Σ[ p ∈ List A ] (p ++ sf ≡ w)
Suffix⇒split here      = [] , refl
Suffix⇒split (there {x = x} p) with Suffix⇒split p
... | (pre , refl) = x ∷ pre , refl

suffix-prefix-infix : {a b full : List Char} → a Suffix b →
                      Σ[ t ∈ List Char ] (b ++ t ≡ full) → a Infix full
suffix-prefix-infix {a} {b} {full} hs (t , ht) with Suffix⇒split hs
... | (p , refl) =
  infix-mk p t (trans (sym (++-assoc p a t)) ht)

record Realized (full : List Char) (b : ℕ) : Set where
  constructor realized-mk
  field
    rsub  : List Char
    rinf  : rsub Infix full
    rval  : Distinct rsub
    rlen  : length rsub ≡ b
open Realized

goVReal : (full rem win : List Char) (best : ℕ) (P : List Char) →
          P ++ rem ≡ full → win Suffix P → Distinct win →
          Realized full best →
          Realized full (goV best win rem)
goVReal full []       win best P hPrem hwP hwv real = real
goVReal full (c ∷ cs) win best P hPrem hwP hwv real =
  goVReal full cs (shrinkV (win ++ (c ∷ [])))
          (best ⊔ length (shrinkV (win ++ (c ∷ [])))) (P ++ (c ∷ []))
          hP'full hwP' (shrinkV-valid (win ++ (c ∷ []))) real'
  where
    hP'full : (P ++ (c ∷ [])) ++ cs ≡ full
    hP'full = trans (++-assoc P (c ∷ []) cs) hPrem

    hwP' : shrinkV (win ++ (c ∷ [])) Suffix (P ++ (c ∷ []))
    hwP' = Suffix-trans (shrinkV-suffix (win ++ (c ∷ [])))
                        (suffix-snoc-mono c hwP)

    hwinf : shrinkV (win ++ (c ∷ [])) Infix full
    hwinf = suffix-prefix-infix hwP' (cs , hP'full)

    real' : Realized full (best ⊔ length (shrinkV (win ++ (c ∷ []))))
    real' with ≤-total (length (shrinkV (win ++ (c ∷ [])))) best
    ... | inj₁ wlen≤best =
      realized-mk (rsub real) (rinf real) (rval real)
                  (trans (rlen real) (sym (m≥n⇒m⊔n≡m wlen≤best)))
    ... | inj₂ best≤wlen =
      realized-mk (shrinkV (win ++ (c ∷ []))) hwinf
                  (shrinkV-valid (win ++ (c ∷ [])))
                  (sym (m≤n⇒m⊔n≡n best≤wlen))

nil-Infix : (full : List Char) → ([] {A = Char}) Infix full
nil-Infix full = infix-mk [] full refl

-- REALIZABILITY: the window's answer is realized by an actual all-distinct substring.
window-realizable : (s : String) → Realized (toList s) (lengthOfLongestV s)
window-realizable s =
  goVReal (toList s) (toList s) [] 0 [] refl here distinct-nil
          (realized-mk [] (nil-Infix (toList s)) distinct-nil refl)

------------------------------------------------------------------------
-- FULL OPTIMALITY: `lengthOfLongestV s` is EXACTLY the length of the
-- longest all-distinct substring — realizability + the lower bound.
------------------------------------------------------------------------

lengthOfLongestV-optimal :
  (s : String) →
  (Σ[ sub ∈ List Char ]
     (sub Infix (toList s) × Distinct sub × length sub ≡ lengthOfLongestV s))
  ×
  ((sub : List Char) → sub Infix (toList s) → Distinct sub →
     length sub ≤ lengthOfLongestV s)
lengthOfLongestV-optimal s =
  (rsub R , rinf R , rval R , rlen R)
  , (λ sub hi hv → window-optimal s sub hi hv)
  where
    R : Realized (toList s) (lengthOfLongestV s)
    R = window-realizable s

------------------------------------------------------------------------
-- ════════════════════════════════════════════════════════════════════
-- EQUIVALENCE: the fast last-seen-index solution `lengthOfLongest`
-- computes the same value as the verified shrink-window
-- `lengthOfLongestV`.  So the optimality proof above transfers to the
-- shipped algorithm.  (Port of the Lean `goFast_eq_goV` simulation.)
--
-- Key fact used throughout: in agda-stdlib, `Data.Char._==_` is defined as
-- `c == d = isYes (c ≟ d)`, so a `with c ≟ d` simultaneously reduces every
-- `if c == d …` in the goal.
-- ════════════════════════════════════════════════════════════════════

-- index of the LAST occurrence of c in xs (positions from 0), as a Maybe
lastIdx : Char → List Char → Maybe ℕ
lastIdx c []       = nothing
lastIdx c (x ∷ xs) with lastIdx c xs
... | just j  = just (suc j)
... | nothing = if c == x then just 0 else nothing

-- setIdx then lookupIdx behaves like a functional update
lookupIdx-setIdx : (c d : Char) (i : ℕ) (seen : List (Char × ℕ)) →
    lookupIdx d (setIdx c i seen) ≡ (if d == c then just i else lookupIdx d seen)
lookupIdx-setIdx c d i []       with d ≟ c
... | yes _ = refl
... | no  _ = refl
lookupIdx-setIdx c d i ((e , j) ∷ rest) with c ≟ e
-- c == e : setIdx replaces head with (c,i).  lookupIdx d ((c,i)∷rest):
--   if d == c then just i else lookupIdx d rest.   We must also rewrite the
--   target's `lookupIdx d ((e,j)∷rest)` head-check d==e to d==c (since e=c).
lookupIdx-setIdx c d i ((e , j) ∷ rest) | yes refl with d ≟ e
...   | yes refl = refl
...   | no  _    = refl
lookupIdx-setIdx c d i ((e , j) ∷ rest) | no c≢e with d ≟ e
-- c ≢ e, head unchanged = (e,j); if d == e then just j, both sides agree
...   | yes refl with d ≟ c
...     | yes refl = ⊥-elim (c≢e refl)
...     | no  _    = refl
lookupIdx-setIdx c d i ((e , j) ∷ rest) | no c≢e | no d≢e =
  lookupIdx-setIdx c d i rest

-- lastIdx after appending: the new char becomes the last index; others unchanged
lastIdx-snoc-self : (c : Char) (P : List Char) →
                    lastIdx c (P ++ (c ∷ [])) ≡ just (length P)
lastIdx-snoc-self c []       with c ≟ c
... | yes _   = refl
... | no  c≢c = ⊥-elim (c≢c refl)
lastIdx-snoc-self c (x ∷ xs) rewrite lastIdx-snoc-self c xs = refl

lastIdx-snoc-other : (c d : Char) (P : List Char) → ¬ (d ≡ c) →
                     lastIdx d (P ++ (c ∷ [])) ≡ lastIdx d P
lastIdx-snoc-other c d []       d≢c with d ≟ c
... | yes p = ⊥-elim (d≢c p)
... | no  _ = refl
lastIdx-snoc-other c d (x ∷ xs) d≢c
  rewrite lastIdx-snoc-other c d xs d≢c = refl

-- lastIdx of a cons, made explicit so proofs can reason about it.
lastIdx-cons-just : (c x : Char) (xs : List Char) (j : ℕ) → lastIdx c xs ≡ just j →
                    lastIdx c (x ∷ xs) ≡ just (suc j)
lastIdx-cons-just c x xs j e rewrite e = refl

lastIdx-cons-none-≡ : (c x : Char) (xs : List Char) → lastIdx c xs ≡ nothing → c ≡ x →
                      lastIdx c (x ∷ xs) ≡ just 0
lastIdx-cons-none-≡ c x xs e c≡x rewrite e with c ≟ x
... | yes _   = refl
... | no  c≢x = ⊥-elim (c≢x c≡x)

lastIdx-cons-none-≢ : (c x : Char) (xs : List Char) → lastIdx c xs ≡ nothing → ¬ (c ≡ x) →
                      lastIdx c (x ∷ xs) ≡ nothing
lastIdx-cons-none-≢ c x xs e c≢x rewrite e with c ≟ x
... | yes p   = ⊥-elim (c≢x p)
... | no  _   = refl

-- if lastIdx c (x∷xs) = nothing then lastIdx c xs = nothing (the cons can only
-- add an index, never remove one)
lastIdx-cons-none⇒tail-none : (c x : Char) (xs : List Char) →
                              lastIdx c (x ∷ xs) ≡ nothing → lastIdx c xs ≡ nothing
lastIdx-cons-none⇒tail-none c x xs hh with lastIdx c xs
... | nothing = refl
... | just j  = ⊥-elim (case hh of λ ())

-- lastIdx c xs = nothing  ⇒  c does not occur.
lastIdx-none⇒¬elem : (c : Char) (xs : List Char) →
                     lastIdx c xs ≡ nothing → decElem c xs ≡ false
lastIdx-none⇒¬elem c []       _ = refl
lastIdx-none⇒¬elem c (x ∷ xs) h with c ≟ x
-- c ≡ x : with tail nothing, lastIdx c (x∷xs) = just 0 ≠ nothing — contradiction.
... | yes c≡x =
  ⊥-elim (case (trans (sym (lastIdx-cons-none-≡ c x xs
                              (lastIdx-cons-none⇒tail-none c x xs h) c≡x)) h) of λ ())
-- c ≢ x : decElem c (x∷xs) reduces to decElem c xs; lastIdx c xs = nothing by the tail.
... | no c≢x = lastIdx-none⇒¬elem c xs (lastIdx-cons-none⇒tail-none c x xs h)

-- c does not occur  ⇒  lastIdx c xs = nothing
¬elem⇒lastIdx-none : (c : Char) (xs : List Char) →
                     decElem c xs ≡ false → lastIdx c xs ≡ nothing
¬elem⇒lastIdx-none c []       _ = refl
¬elem⇒lastIdx-none c (x ∷ xs) h with c ≟ x
-- c ≡ x : decElem c (x∷xs) reduces to true, contradicting h ≡ false
... | yes _ = ⊥-elim (case h of λ ())
-- c ≢ x : h reduces to decElem c xs ≡ false; recurse to get lastIdx c xs = nothing,
-- then lastIdx-cons-none-≢ gives lastIdx c (x∷xs) = nothing.
... | no  c≢x = lastIdx-cons-none-≢ c x xs (¬elem⇒lastIdx-none c xs h) c≢x

-- a last-occurrence index is always within bounds
just-injective : {a b : ℕ} → (just a ≡ just b) → a ≡ b
just-injective refl = refl

lastIdx-lt : (c : Char) (xs : List Char) (j : ℕ) → lastIdx c xs ≡ just j → j < length xs
lastIdx-lt c (x ∷ xs) j h with lastIdx c xs in eqt
-- tail gives just k ⇒ lastIdx c (x∷xs) reduces to just (suc k), so h : just (suc k) ≡ just j
... | just k =
  subst (_< length (x ∷ xs)) (just-injective h) (s≤s (lastIdx-lt c xs k eqt))
-- tail nothing ⇒ lastIdx c (x∷xs) = if c==x then just 0 else nothing
... | nothing with c ≟ x
-- c ≡ x : reduces to just 0, so h : just 0 ≡ just j ⇒ j = 0 < length (x∷xs)
...   | yes c≡x = subst (_< length (x ∷ xs)) (just-injective h) (s≤s z≤n)
-- c ≢ x : reduces to nothing, so h : nothing ≡ just j is absurd
...   | no  c≢x = ⊥-elim (case h of λ ())

-- last occurrence index in an append, expressed via a small eliminator
appendIdx : ℕ → Maybe ℕ → Maybe ℕ → Maybe ℕ
appendIdx lenxs (just j) lastxs = just (lenxs + j)
appendIdx lenxs nothing  lastxs = lastxs

lastIdx-append : (c : Char) (xs ys : List Char) →
                 lastIdx c (xs ++ ys) ≡ appendIdx (length xs) (lastIdx c ys) (lastIdx c xs)
lastIdx-append c [] ys with lastIdx c ys
... | just j  = refl
... | nothing = refl
lastIdx-append c (x ∷ xs) ys rewrite lastIdx-append c xs ys with lastIdx c ys
... | just j  = refl
... | nothing = refl

-- how far the window start must jump when re-appending c (0 if c is new)
dropAmtFromIdx : Maybe ℕ → ℕ
dropAmtFromIdx (just j) = suc j
dropAmtFromIdx nothing  = 0

dropAmt : List Char → Char → ℕ
dropAmt win c = dropAmtFromIdx (lastIdx c win)

------------------------------------------------------------------------
-- Distinct ⇄ decElem helpers (needed by the crux `shrinkV-snoc-drop`).
------------------------------------------------------------------------

-- Distinct (x ∷ xs)  ⇒  decElem x xs ≡ false  (a repeated head breaks distinctness)
distinct-cons⇒¬elem : (x : Char) (xs : List Char) → Distinct (x ∷ xs) → decElem x xs ≡ false
distinct-cons⇒¬elem x xs h with decElem x xs | nd-le-length xs
-- decElem x xs = true : nd (x∷xs) = nd xs ≤ length xs < suc (length xs) = length (x∷xs);
-- but Distinct says nd (x∷xs) = length (x∷xs) — contradiction.
... | true  | hle = ⊥-elim (1+n≰n (subst (_≤ length xs) h hle))
... | false | _   = refl

-- nd of a cons with a fresh head
nd-cons-false : (x : Char) (xs : List Char) → decElem x xs ≡ false →
                nd (x ∷ xs) ≡ suc (nd xs)
nd-cons-false x xs he rewrite he = refl

-- Distinct (x ∷ xs)  ⇒  Distinct xs
distinct-cons⇒tail : (x : Char) (xs : List Char) → Distinct (x ∷ xs) → Distinct xs
distinct-cons⇒tail x xs h =
  suc-injective (trans (sym (nd-cons-false x xs (distinct-cons⇒¬elem x xs h))) h)

-- the converse: a fresh char on the front of a distinct list stays distinct
¬elem×distinct⇒cons : (x : Char) (xs : List Char) →
                      decElem x xs ≡ false → Distinct xs → Distinct (x ∷ xs)
¬elem×distinct⇒cons x xs he hd =
  trans (nd-cons-false x xs he) (cong suc hd)

-- a fresh char appended to a distinct list stays distinct
distinct-snoc : (c : Char) (xs : List Char) → decElem c xs ≡ false → Distinct xs →
                Distinct (xs ++ (c ∷ []))
distinct-snoc c xs hc hd
  rewrite nd-snoc c xs | hc | length-snoc xs c = cong suc hd

-- appending c keeps the list distinct only if c is new
distinct-snoc⇒¬elem : (c : Char) (xs : List Char) → Distinct (xs ++ (c ∷ [])) →
                      decElem c xs ≡ false
distinct-snoc⇒¬elem c xs h with decElem c xs in eqc | nd-le-length xs
-- decElem c xs = true : nd (xs++[c]) = nd xs ≤ length xs < suc (length xs) = length (xs++[c])
... | true  | hle =
  ⊥-elim (1+n≰n (subst (_≤ length xs)
           (trans (sym (nd-snoc-true c xs eqc)) (trans h (length-snoc xs c))) hle))
  where
    nd-snoc-true : (c : Char) (xs : List Char) → decElem c xs ≡ true →
                   nd (xs ++ (c ∷ [])) ≡ nd xs
    nd-snoc-true c xs ev rewrite nd-snoc c xs | ev = refl
... | false | _   = refl

------------------------------------------------------------------------
-- shrinkV is idempotent under "re-append and re-shrink".
------------------------------------------------------------------------

-- two suffixes of the same list, each the longest distinct one, are equal
Suffix-antisym : {sf w : List Char} → sf Suffix w → w Suffix sf → sf ≡ w
Suffix-antisym here      _ = refl
Suffix-antisym (there p) q =
  ⊥-elim (1+n≰n (≤-trans (Suffix-length q) (Suffix-length p)))

shrinkV-idem : (P : List Char) (c : Char) →
               shrinkV (shrinkV P ++ (c ∷ [])) ≡ shrinkV (P ++ (c ∷ []))
shrinkV-idem P c = Suffix-antisym h1 h2
  where
    -- shrinkV (shrinkV P ++ [c]) is a distinct suffix of shrinkV P ++ [c],
    -- which is a suffix of P ++ [c]; so it sits inside shrinkV (P ++ [c]).
    h1 : shrinkV (shrinkV P ++ (c ∷ [])) Suffix shrinkV (P ++ (c ∷ []))
    h1 = shrinkV-longest (shrinkV (shrinkV P ++ (c ∷ [])))
           (shrinkV-valid (shrinkV P ++ (c ∷ []))) (P ++ (c ∷ []))
           (Suffix-trans (shrinkV-suffix (shrinkV P ++ (c ∷ [])))
                         (suffix-snoc-mono c (shrinkV-suffix P)))
    -- conversely shrinkV (P ++ [c]) is a distinct suffix of P ++ [c]; decompose it.
    h2 : shrinkV (P ++ (c ∷ [])) Suffix shrinkV (shrinkV P ++ (c ∷ []))
    h2 with suffix-snoc-decomp {xs = P} (shrinkV-suffix (P ++ (c ∷ [])))
    ... | inj₁ sf≡[] =
      subst (_Suffix shrinkV (shrinkV P ++ (c ∷ []))) (sym sf≡[]) (nil-Suffix _)
    ... | inj₂ (sf0 , sf≡sf0c , sf0sufP) =
      subst (_Suffix shrinkV (shrinkV P ++ (c ∷ []))) (sym sf≡sf0c)
        (shrinkV-longest (sf0 ++ (c ∷ [])) hsf0c-valid (shrinkV P ++ (c ∷ []))
          (suffix-snoc-mono c
            (shrinkV-longest sf0 (distinct-init sf0 c hsf0c-valid) P sf0sufP)))
      where
        -- sf0 ++ [c] is distinct because it equals shrinkV (P ++ [c]) which is distinct
        hsf0c-valid : Distinct (sf0 ++ (c ∷ []))
        hsf0c-valid =
          subst Distinct sf≡sf0c (shrinkV-valid (P ++ (c ∷ [])))

------------------------------------------------------------------------
-- THE CRUX: for a distinct window, appending c and re-shrinking = dropping
-- past c's last occurrence (or nothing, if c is new).
------------------------------------------------------------------------

-- shrinkV keeps a distinct cons whole
shrinkV-keep : (x : Char) (xs : List Char) → distinctB (x ∷ xs) ≡ true →
               shrinkV (x ∷ xs) ≡ x ∷ xs
shrinkV-keep x xs h rewrite h = refl

-- shrinkV of a non-distinct cons drops the head
shrinkV-drop : (x : Char) (xs : List Char) → distinctB (x ∷ xs) ≡ false →
               shrinkV (x ∷ xs) ≡ shrinkV xs
shrinkV-drop x xs h rewrite h = refl

-- dropAmt of a cons when c does occur in the tail (lastIdx tail = just k)
dropAmt-cons-just : (c x : Char) (w : List Char) (k : ℕ) → lastIdx c w ≡ just k →
                    dropAmt (x ∷ w) c ≡ suc (dropAmt w c)
dropAmt-cons-just c x w k hk
  rewrite hk | lastIdx-cons-just c x w k hk = refl

-- if x ∷ (w ++ [c]) is distinct (c new at the end and x not in w++[c]) then c is
-- fresh in x ∷ w, so dropAmt (x∷w) c = 0
dropAmt-fresh : (c x : Char) (w : List Char) →
                lastIdx c (x ∷ w) ≡ nothing → dropAmt (x ∷ w) c ≡ 0
dropAmt-fresh c x w h rewrite h = refl

-- When c is fresh in x ∷ w (lastIdx nothing), dropAmt (x∷w) c = 0.
dropAmt-fresh-cons : (c x : Char) (w : List Char) → lastIdx c (x ∷ w) ≡ nothing →
                     dropAmt (x ∷ w) c ≡ 0
dropAmt-fresh-cons c x w h rewrite h = refl

-- the cons-step of dropAmt in the not-distinct case, given lastIdx c w
dropAmt-step-lemma : (c x : Char) (w : List Char) →
                     distinctB (x ∷ (w ++ (c ∷ []))) ≡ false → Distinct (x ∷ w) →
                     dropAmt (x ∷ w) c ≡ suc (dropAmt w c)
dropAmt-step-lemma c x w eqd hwin with lastIdx c w in eqw
-- c occurs in w (at k): lastIdx c (x∷w) = just (suc k), dropAmt steps by one
... | just k  rewrite eqw | lastIdx-cons-just c x w k eqw = refl
-- c not in w: dropAmt w c = 0; must show dropAmt (x∷w) c = 1, i.e. c ≡ x.
... | nothing with c ≟ x
-- c ≡ x : lastIdx c (x∷w) = just 0, dropAmt (x∷w) c = 1 = suc 0
...   | yes c≡x rewrite eqw | lastIdx-cons-none-≡ c x w eqw c≡x = refl
-- c ≢ x AND c ∉ w : x∷(w++[c]) is distinct, contradicting eqd ≡ false
...   | no  c≢x = ⊥-elim (false≢true (trans (sym eqd)
                    (Distinct⇒distinctB (x ∷ (w ++ (c ∷ []))) built)))
  where
    false≢true : false ≡ true → ⊥
    false≢true ()
    c∉w : decElem c w ≡ false
    c∉w = lastIdx-none⇒¬elem c w eqw
    x∉w : decElem x w ≡ false
    x∉w = distinct-cons⇒¬elem x w hwin
    x∉wc : decElem x (w ++ (c ∷ [])) ≡ false
    x∉wc = trans (decElem-snoc-neq x c w (≟-sym c x c≢x)) x∉w
    built : Distinct (x ∷ (w ++ (c ∷ [])))
    built = ¬elem×distinct⇒cons x (w ++ (c ∷ [])) x∉wc
              (distinct-snoc c w c∉w (distinct-cons⇒tail x w hwin))

-- in the distinct (kept-whole) case c is fresh, so dropAmt (x∷w) c = 0
dropAmt-keep-lemma : (c x : Char) (w : List Char) →
                     distinctB (x ∷ (w ++ (c ∷ []))) ≡ true →
                     dropAmt (x ∷ w) c ≡ 0
dropAmt-keep-lemma c x w eqd = dropAmt-fresh-cons c x w c-fresh
  where
    hd : Distinct (x ∷ (w ++ (c ∷ [])))
    hd = distinctB⇒Distinct (x ∷ (w ++ (c ∷ []))) eqd
    x∉wc : decElem x (w ++ (c ∷ [])) ≡ false
    x∉wc = distinct-cons⇒¬elem x (w ++ (c ∷ [])) hd
    c∉w : decElem c w ≡ false
    c∉w = distinct-snoc⇒¬elem c w (distinct-cons⇒tail x (w ++ (c ∷ [])) hd)
    c≢x : ¬ (c ≡ x)
    c≢x c≡x with trans (sym (subst (λ z → decElem z (w ++ (c ∷ [])) ≡ true)
                                   c≡x (decElem-snoc-self c w))) x∉wc
    ... | ()
    c∉xw : decElem c (x ∷ w) ≡ false
    c∉xw = trans (decElem-cons-neq c x w c≢x) c∉w
    c-fresh : lastIdx c (x ∷ w) ≡ nothing
    c-fresh = ¬elem⇒lastIdx-none c (x ∷ w) c∉xw

-- the cons case, with the distinctB value supplied so the goal is NOT abstracted
shrinkV-snoc-drop-cons :
  (c x : Char) (w : List Char) → Distinct (x ∷ w) →
  (b : Bool) → distinctB (x ∷ (w ++ (c ∷ []))) ≡ b →
  -- IH for the tail
  (shrinkV (w ++ (c ∷ [])) ≡ drop (dropAmt w c) (w ++ (c ∷ []))) →
  shrinkV ((x ∷ w) ++ (c ∷ [])) ≡ drop (dropAmt (x ∷ w) c) ((x ∷ w) ++ (c ∷ []))
shrinkV-snoc-drop-cons c x w hwin true  eqd ih =
  trans (shrinkV-keep x (w ++ (c ∷ [])) eqd)
        (cong (λ n → drop n ((x ∷ w) ++ (c ∷ []))) (sym (dropAmt-keep-lemma c x w eqd)))
shrinkV-snoc-drop-cons c x w hwin false eqd ih =
  trans (trans (shrinkV-drop x (w ++ (c ∷ [])) eqd) ih)
        (sym (cong (λ n → drop n ((x ∷ w) ++ (c ∷ [])))
                   (dropAmt-step-lemma c x w eqd hwin)))

shrinkV-snoc-drop : (c : Char) (win : List Char) → Distinct win →
                    shrinkV (win ++ (c ∷ [])) ≡ drop (dropAmt win c) (win ++ (c ∷ []))
shrinkV-snoc-drop c []       hwin = refl
shrinkV-snoc-drop c (x ∷ w)  hwin =
  shrinkV-snoc-drop-cons c x w hwin (distinctB (x ∷ (w ++ (c ∷ [])))) refl
    (shrinkV-snoc-drop c w (distinct-cons⇒tail x w hwin))

------------------------------------------------------------------------
-- The simulation: goFast tracks the same window as goV.
------------------------------------------------------------------------

-- `seen` correctly records each char's last index in the consumed prefix P
SeenOk : List (Char × ℕ) → List Char → Set
SeenOk seen P = (d : Char) → lookupIdx d seen ≡ lastIdx d P

seenOk-step : (seen : List (Char × ℕ)) (P : List Char) (c : Char) (i : ℕ) →
              i ≡ length P → SeenOk seen P → SeenOk (setIdx c i seen) (P ++ (c ∷ []))
seenOk-step seen P c i hi h d rewrite lookupIdx-setIdx c d i seen with d ≟ c
-- d ≡ c : both sides give just (length P) = just i
... | yes refl rewrite lastIdx-snoc-self c P | hi = refl
-- d ≢ c : lookupIdx unchanged; lastIdx ignores the appended c
... | no  d≢c = trans (h d) (sym (lastIdx-snoc-other c d P d≢c))

-- drop past a prefix when n ≤ |l₁|
drop-append-le : {l₁ l₂ : List Char} {n : ℕ} → n ≤ length l₁ →
                 drop n (l₁ ++ l₂) ≡ drop n l₁ ++ l₂
drop-append-le {[]}     {l₂} {zero}  _       = refl
drop-append-le {x ∷ l₁} {l₂} {zero}  _       = refl
drop-append-le {x ∷ l₁} {l₂} {suc n} (s≤s p) = drop-append-le {l₁} {l₂} {n} p

------------------------------------------------------------------------
-- start-conn: the fast `start'` = `start` plus how far the shrink drops.
------------------------------------------------------------------------

length-take-≤ : (n : ℕ) (xs : List Char) → n ≤ length xs → length (take n xs) ≡ n
length-take-≤ n xs h = trans (length-take n xs) (m≤n⇒m⊓n≡m h)

-- jump on the actual last-occurrence index of c in P, given that index `md`,
-- the active-window index `da`, the prefix index `ma`, and that c's last
-- occurrence in P is appendIdx start da ma.
-- We case on the active-window index `da` explicitly to avoid abstracting the
-- top-level goal.
start-conn-aux :
  (P : List Char) (start : ℕ) (c : Char) → start ≤ length P →
  (da : Maybe ℕ) → lastIdx c (drop start P) ≡ da →
  (ma : Maybe ℕ) → lastIdx c (take start P) ≡ ma →
  lastIdx c P ≡ appendIdx start da ma →
  jump start (lastIdx c P) ≡ start + dropAmt (drop start P) c
-- c occurs in active window at k: lastIdx c P = just (start+k), dropAmt = suc k
start-conn-aux P start c hstart (just k) eqd ma eqt lastIdxP
  rewrite eqd =
  trans (cong (jump start) lastIdxP)
        (trans (m≤n⇒m⊔n≡n (m≤n⇒m≤1+n (m≤m+n start k))) (sym (+-suc start k)))
-- c not in active window
start-conn-aux P start c hstart nothing eqd (just j) eqt lastIdxP
  rewrite eqd =
  -- c occurs earlier at j < start ⇒ jump keeps start, dropAmt = 0
  trans (cong (jump start) lastIdxP)
        (trans (m≥n⇒m⊔n≡m j<start) (sym (+-identityʳ start)))
  where
    j<start : j < start
    j<start = subst (j <_) (length-take-≤ start P hstart)
                (lastIdx-lt c (take start P) j eqt)
start-conn-aux P start c hstart nothing eqd nothing eqt lastIdxP
  rewrite eqd =
  -- c occurs nowhere ⇒ jump = start, dropAmt = 0
  trans (cong (jump start) lastIdxP) (sym (+-identityʳ start))

start-conn : (P : List Char) (start : ℕ) (c : Char) → start ≤ length P →
             jump start (lastIdx c P) ≡ start + dropAmt (drop start P) c
start-conn P start c hstart =
  start-conn-aux P start c hstart
    (lastIdx c (drop start P)) refl
    (lastIdx c (take start P)) refl
    lastIdxP
  where
    -- lastIdx c P = appendIdx start (lastIdx c (drop start P)) (lastIdx c (take start P))
    lastIdxP : lastIdx c P ≡ appendIdx start (lastIdx c (drop start P)) (lastIdx c (take start P))
    lastIdxP =
      trans (sym (cong (lastIdx c) (take++drop≡id start P)))
            (trans (lastIdx-append c (take start P) (drop start P))
                   (cong (λ s → appendIdx s (lastIdx c (drop start P)) (lastIdx c (take start P)))
                         (length-take-≤ start P hstart)))

------------------------------------------------------------------------
-- window-step: the new window stays the longest distinct suffix, at index
-- start + dropAmt.
------------------------------------------------------------------------

window-step : (P : List Char) (start : ℕ) (c : Char) → start ≤ length P →
              shrinkV P ≡ drop start P →
              shrinkV (P ++ (c ∷ [])) ≡ drop (start + dropAmt (drop start P) c) (P ++ (c ∷ []))
window-step P start c hstart hwin =
  trans (sym (shrinkV-idem P c))
  (trans (cong (λ z → shrinkV (z ++ (c ∷ []))) hwin)
  (trans (shrinkV-snoc-drop c (drop start P) (subst Distinct hwin (shrinkV-valid P)))
  (trans (cong (drop (dropAmt (drop start P) c)) (sym (drop-append-le {P} {c ∷ []} {start} hstart)))
         (drop-drop start (dropAmt (drop start P) c) (P ++ (c ∷ []))))))

------------------------------------------------------------------------
-- goFast simulates goV: the fast index window tracks shrinkV.
------------------------------------------------------------------------

-- length of the new window equals (i+1) - start'
window-len : (P : List Char) (start : ℕ) (c : Char) (i : ℕ) → i ≡ length P →
             start ≤ length P → shrinkV P ≡ drop start P →
             length (shrinkV (P ++ (c ∷ []))) ≡ suc i ∸ (start + dropAmt (drop start P) c)
window-len P start c i hi hstart hwin =
  trans (cong length (window-step P start c hstart hwin))
  (trans (length-drop (start + dropAmt (drop start P) c) (P ++ (c ∷ [])))
         (cong (λ n → n ∸ (start + dropAmt (drop start P) c))
               (trans (length-snoc P c) (cong suc (sym hi)))))

goFast-eq-goV : (rem : List Char) (P : List Char) (i start : ℕ)
                (seen : List (Char × ℕ)) (best : ℕ) →
                i ≡ length P → start ≤ length P → shrinkV P ≡ drop start P →
                SeenOk seen P →
                goFast i start seen best rem ≡ goV best (shrinkV P) rem
goFast-eq-goV []       P i start seen best hi hstart hwin hseen = refl
goFast-eq-goV (c ∷ cs) P i start seen best hi hstart hwin hseen =
  trans (cong (λ q → goFast (suc i) (jump start q)
                            (setIdx c i seen) (best ⊔ (suc i ∸ jump start q)) cs)
              (hseen c))
        (trans
          (cong (λ st → goFast (suc i) st (setIdx c i seen) (best ⊔ (suc i ∸ st)) cs)
                start-conn-eq)
          (trans
            (goFast-eq-goV cs (P ++ (c ∷ [])) (suc i) start' (setIdx c i seen)
              (best ⊔ (suc i ∸ start'))
              (trans (cong suc hi) (sym (length-snoc P c)))
              hstart' hwin' hseen')
            (trans
              (cong (λ best2 → goV best2 (shrinkV (P ++ (c ∷ []))) cs)
                    (cong (best ⊔_) (sym (window-len P start c i hi hstart hwin))))
              -- rewrite shrinkV (P ++ [c])  ↦  shrinkV (shrinkV P ++ [c])  via idempotence,
              -- so this matches `goV best (shrinkV P) (c ∷ cs)` unfolded one step.
              (cong (λ w → goV (best ⊔ length w) w cs) (sym (shrinkV-idem P c))))))
  where
    start' : ℕ
    start' = start + dropAmt (drop start P) c
    start-conn-eq : jump start (lastIdx c P) ≡ start'
    start-conn-eq = start-conn P start c hstart
    hwin' : shrinkV (P ++ (c ∷ [])) ≡ drop start' (P ++ (c ∷ []))
    hwin' = window-step P start c hstart hwin
    hstart' : start' ≤ length (P ++ (c ∷ []))
    hstart' = subst (start' ≤_) (sym (length-snoc P c))
                (≤-trans hstart-le-Pc (n≤1+n (length P)))
      where
        -- start' = start + dropAmt; dropAmt ≤ |drop start P| = |P| - start, so start' ≤ |P|
        dropAmt≤ : dropAmt (drop start P) c ≤ length (drop start P)
        dropAmt≤ with lastIdx c (drop start P) in eqdd
        ... | just k  = lastIdx-lt c (drop start P) k eqdd
        ... | nothing = z≤n
        -- start + dropAmt ≤ start + (|P| ∸ start) = |P|, since dropAmt ≤ |drop start P| = |P| ∸ start
        start'≤P : start' ≤ length P
        start'≤P = subst (start' ≤_) (m+[n∸m]≡n hstart)
                     (+-monoʳ-≤ start
                        (subst (dropAmt (drop start P) c ≤_) (length-drop start P) dropAmt≤))
        hstart-le-Pc : start' ≤ length P
        hstart-le-Pc = start'≤P
    hseen' : SeenOk (setIdx c i seen) (P ++ (c ∷ []))
    hseen' = seenOk-step seen P c i hi hseen

------------------------------------------------------------------------
-- EQUIVALENCE: the fast last-seen-index solution `lengthOfLongest`
-- equals the verified window `lengthOfLongestV`.  Instantiate the
-- simulation at the empty consumed prefix.  `shrinkV []` reduces to `[]`
-- definitionally, so `goV 0 (shrinkV []) (toList s) = lengthOfLongestV s`.
------------------------------------------------------------------------

lengthOfLongest-eq : (s : String) → lengthOfLongest s ≡ lengthOfLongestV s
lengthOfLongest-eq s =
  goFast-eq-goV (toList s) [] 0 0 [] 0 refl z≤n refl (λ d → refl)

------------------------------------------------------------------------
-- PAYOFF: the SHIPPED `lengthOfLongest` is EXACTLY the length of the
-- longest all-distinct substring (optimality transferred via the
-- equivalence).  Realizability + the lower bound.
------------------------------------------------------------------------

lengthOfLongest-optimal :
  (s : String) →
  (Σ[ sub ∈ List Char ]
     (sub Infix (toList s) × Distinct sub × length sub ≡ lengthOfLongest s))
  ×
  ((sub : List Char) → sub Infix (toList s) → Distinct sub →
     length sub ≤ lengthOfLongest s)
lengthOfLongest-optimal s =
  (rsub R , rinf R , rval R , trans (rlen R) (sym (lengthOfLongest-eq s)))
  , (λ sub hi hv → subst (length sub ≤_) (sym (lengthOfLongest-eq s))
                          (window-optimal s sub hi hv))
  where
    R : Realized (toList s) (lengthOfLongestV s)
    R = window-realizable s
