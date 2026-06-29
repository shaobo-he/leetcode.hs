module Main

------------------------------------------------------------------------
-- LeetCode 208: Implement Trie (Prefix Tree).
--
-- The alphabet is a SMALL inductive type `Sym`, so equality is decidable and
-- every function below is `total` (no `Char` coverage gaps).  A trie is
-- `Node isEnd children` with assoc-list children; insert/search/startsWith all
-- recurse structurally on the key word.  `build = foldr insert empty`.
--
-- Refinement (ported from solution.lean, fully proved below, no postulates):
--   searchBuild     : search w (build ws)      = elemW w ws
--   startsWithBuild : startsWith p (build ws)  = (isNil p || anyPrefix p ws)
-- where `elemW` is boolean list membership and `anyPrefix p` tests whether some
-- word in the list has `p` as a prefix.  The `isNil p ||` guard is MANDATORY:
-- the bare "some word has p as a prefix" is FALSE at (ws = [], p = []), yet
-- `startsWith [] t = True`.
--
-- The proofs mirror the Lean double-structural inductions (searchInsert /
-- startsWithInsert over a single `insert`, then induction on `ws` for `build`),
-- resting on assoc-list lookup hit/miss lemmas (lookupChildInsertSelf/Other).
-- `symEq` soundness (symEqSound : symEq a b = True -> a = b) bridges the boolean
-- alphabet equality with propositional `=`.  Two Idris-specific devices keep
-- every step total and `Refl`-clean: (1) explicit `*ConsEq/ConsNeq` computation
-- lemmas force the `if symEq ...` reductions, and (2) a single named
-- `searchStepM`/`startsWithStepM` over `Maybe Trie` is used instead of anonymous
-- `case` expressions, sidestepping Idris' case-lift identity problem.
------------------------------------------------------------------------

data Sym = A | B | C | D | E | L | P

-- decidable / boolean equality on the small alphabet
symEq : Sym -> Sym -> Bool
symEq A A = True
symEq B B = True
symEq C C = True
symEq D D = True
symEq E E = True
symEq L L = True
symEq P P = True
symEq _ _ = False

Word : Type
Word = List Sym

wordEq : Word -> Word -> Bool
wordEq []        []        = True
wordEq (x :: xs) (y :: ys) = symEq x y && wordEq xs ys
wordEq _         _         = False

data Trie = Node Bool (List (Sym, Trie))

emptyTrie : Trie
emptyTrie = Node False []

------------------------------------------------------------------------
-- insert (mutual with the child-list update).  `total`: size-change sees the
-- key word strictly shrink on the insert -> childInsert step of every cycle.
------------------------------------------------------------------------

mutual
  total
  insert : Word -> Trie -> Trie
  insert []        (Node _ cs) = Node True cs
  insert (b :: vs) (Node e cs) = Node e (childInsert b vs cs)

  total
  childInsert : Sym -> Word -> List (Sym, Trie) -> List (Sym, Trie)
  childInsert b vs []             = [(b, insert vs emptyTrie)]
  childInsert b vs ((y, t) :: r) =
    if symEq b y then (y, insert vs t) :: r
                 else (y, t) :: childInsert b vs r

total
childLookup : Sym -> List (Sym, Trie) -> Maybe Trie
childLookup _ []             = Nothing
childLookup x ((y, t) :: r) = if symEq x y then Just t else childLookup x r

total
search : Word -> Trie -> Bool
search []        (Node e _)  = e
search (x :: xs) (Node _ cs) = case childLookup x cs of
  Just t  => search xs t
  Nothing => False

total
startsWith : Word -> Trie -> Bool
startsWith []        _          = True
startsWith (x :: xs) (Node _ cs) = case childLookup x cs of
  Just t  => startsWith xs t
  Nothing => False

total
build : List Word -> Trie
build = foldr insert emptyTrie

total
isNil : Word -> Bool
isNil []       = True
isNil (_ :: _) = False

total
isPrefix : Word -> Word -> Bool
isPrefix []        _         = True
isPrefix (_ :: _)  []        = False
isPrefix (a :: as) (b :: bs) = if symEq a b then isPrefix as bs else False

total
elemW : Word -> List Word -> Bool
elemW w []        = False
elemW w (v :: vs) = wordEq w v || elemW w vs

total
anyPrefix : Word -> List Word -> Bool
anyPrefix p []        = False
anyPrefix p (w :: ws) = isPrefix p w || anyPrefix p ws

total
fromMaybeTrie : Maybe Trie -> Trie
fromMaybeTrie (Just t) = t
fromMaybeTrie Nothing  = emptyTrie

total
lookupD : Sym -> List (Sym, Trie) -> Trie
lookupD x cs = fromMaybeTrie (childLookup x cs)

------------------------------------------------------------------------
-- small boolean helpers
------------------------------------------------------------------------

total
boolCase : (b : Bool) -> Either (b = True) (b = False)
boolCase True  = Left Refl
boolCase False = Right Refl

total
orFalse : (x : Bool) -> (x || False) = x
orFalse True  = Refl
orFalse False = Refl

total
orRot : (a, b, c : Bool) -> ((a || b) || c) = (a || (c || b))
orRot False False False = Refl
orRot False False True  = Refl
orRot False True  False = Refl
orRot False True  True  = Refl
orRot True  False False = Refl
orRot True  False True  = Refl
orRot True  True  False = Refl
orRot True  True  True  = Refl

------------------------------------------------------------------------
-- symEq soundness / reflexivity
------------------------------------------------------------------------

total
symEqRefl : (a : Sym) -> symEq a a = True
symEqRefl A = Refl
symEqRefl B = Refl
symEqRefl C = Refl
symEqRefl D = Refl
symEqRefl E = Refl
symEqRefl L = Refl
symEqRefl P = Refl

total
symEqSound : (a, b : Sym) -> symEq a b = True -> a = b
symEqSound A A _ = Refl
symEqSound B B _ = Refl
symEqSound C C _ = Refl
symEqSound D D _ = Refl
symEqSound E E _ = Refl
symEqSound L L _ = Refl
symEqSound P P _ = Refl
symEqSound A B Refl impossible
symEqSound A C Refl impossible
symEqSound A D Refl impossible
symEqSound A E Refl impossible
symEqSound A L Refl impossible
symEqSound A P Refl impossible
symEqSound B A Refl impossible
symEqSound B C Refl impossible
symEqSound B D Refl impossible
symEqSound B E Refl impossible
symEqSound B L Refl impossible
symEqSound B P Refl impossible
symEqSound C A Refl impossible
symEqSound C B Refl impossible
symEqSound C D Refl impossible
symEqSound C E Refl impossible
symEqSound C L Refl impossible
symEqSound C P Refl impossible
symEqSound D A Refl impossible
symEqSound D B Refl impossible
symEqSound D C Refl impossible
symEqSound D E Refl impossible
symEqSound D L Refl impossible
symEqSound D P Refl impossible
symEqSound E A Refl impossible
symEqSound E B Refl impossible
symEqSound E C Refl impossible
symEqSound E D Refl impossible
symEqSound E L Refl impossible
symEqSound E P Refl impossible
symEqSound L A Refl impossible
symEqSound L B Refl impossible
symEqSound L C Refl impossible
symEqSound L D Refl impossible
symEqSound L E Refl impossible
symEqSound L P Refl impossible
symEqSound P A Refl impossible
symEqSound P B Refl impossible
symEqSound P C Refl impossible
symEqSound P D Refl impossible
symEqSound P E Refl impossible
symEqSound P L Refl impossible

------------------------------------------------------------------------
-- computation lemmas (force the `if symEq` reductions cleanly)
------------------------------------------------------------------------

total
childInsertConsEq : (b, y : Sym) -> (vs : Word) -> (t : Trie) -> (r : List (Sym, Trie)) ->
  symEq b y = True -> childInsert b vs ((y, t) :: r) = (y, insert vs t) :: r
childInsertConsEq b y vs t r h = rewrite h in Refl

total
childInsertConsNeq : (b, y : Sym) -> (vs : Word) -> (t : Trie) -> (r : List (Sym, Trie)) ->
  symEq b y = False -> childInsert b vs ((y, t) :: r) = (y, t) :: childInsert b vs r
childInsertConsNeq b y vs t r h = rewrite h in Refl

total
childLookupConsEq : (x, y : Sym) -> (t : Trie) -> (r : List (Sym, Trie)) ->
  symEq x y = True -> childLookup x ((y, t) :: r) = Just t
childLookupConsEq x y t r h = rewrite h in Refl

total
childLookupConsNeq : (x, y : Sym) -> (t : Trie) -> (r : List (Sym, Trie)) ->
  symEq x y = False -> childLookup x ((y, t) :: r) = childLookup x r
childLookupConsNeq x y t r h = rewrite h in Refl

total
lookupDCong : (x : Sym) -> (cs1, cs2 : List (Sym, Trie)) ->
  childLookup x cs1 = childLookup x cs2 -> lookupD x cs1 = lookupD x cs2
lookupDCong x cs1 cs2 h = cong fromMaybeTrie h

total
lookupDJust : (x : Sym) -> (cs : List (Sym, Trie)) -> (t : Trie) ->
  childLookup x cs = Just t -> lookupD x cs = t
lookupDJust x cs t h = cong fromMaybeTrie h

total
wordEqConsEq : (x, y : Sym) -> (xs, ys : Word) ->
  symEq x y = True -> wordEq (x :: xs) (y :: ys) = wordEq xs ys
wordEqConsEq x y xs ys h = rewrite h in Refl

total
wordEqConsNeq : (x, y : Sym) -> (xs, ys : Word) ->
  symEq x y = False -> wordEq (x :: xs) (y :: ys) = False
wordEqConsNeq x y xs ys h = rewrite h in Refl

total
isPrefixConsEq : (x, y : Sym) -> (xs, ys : Word) ->
  symEq x y = True -> isPrefix (x :: xs) (y :: ys) = isPrefix xs ys
isPrefixConsEq x y xs ys h = rewrite h in Refl

total
isPrefixConsNeq : (x, y : Sym) -> (xs, ys : Word) ->
  symEq x y = False -> isPrefix (x :: xs) (y :: ys) = False
isPrefixConsNeq x y xs ys h = rewrite h in Refl

total
nilOrPrefix : (ps, vs : Word) -> (isNil ps || isPrefix ps vs) = isPrefix ps vs
nilOrPrefix []        vs = Refl
nilOrPrefix (x :: xs) vs = Refl

------------------------------------------------------------------------
-- empty trie facts
------------------------------------------------------------------------

total
searchEmpty : (w : Word) -> search w (Node False []) = False
searchEmpty []        = Refl
searchEmpty (x :: xs) = Refl

-- A SINGLE named "lookup-then-descend" step on `Maybe Trie`.  Routing every
-- proof obligation through this one function (rather than an anonymous `case`)
-- avoids Idris' case-lift identity problem: two textually-identical `case`
-- expressions elaborate to distinct lifted functions that fail to unify.
total
searchStepM : Word -> Maybe Trie -> Bool
searchStepM ws (Just t) = search ws t
searchStepM ws Nothing  = False

total
startsWithStepM : Word -> Maybe Trie -> Bool
startsWithStepM ps (Just t) = startsWith ps t
startsWithStepM ps Nothing  = False

-- unfold search / startsWith on a nonempty key into the lookup step
-- (the end flag is irrelevant, which `with` makes the evaluator commit to)
total
searchCons : (x : Sym) -> (xs : Word) -> (e : Bool) -> (cs : List (Sym, Trie)) ->
  search (x :: xs) (Node e cs) = searchStepM xs (childLookup x cs)
searchCons x xs e cs with (childLookup x cs)
  searchCons x xs e cs | Just t  = Refl
  searchCons x xs e cs | Nothing = Refl

total
startsWithCons : (x : Sym) -> (xs : Word) -> (e : Bool) -> (cs : List (Sym, Trie)) ->
  startsWith (x :: xs) (Node e cs) = startsWithStepM xs (childLookup x cs)
startsWithCons x xs e cs with (childLookup x cs)
  startsWithCons x xs e cs | Just t  = Refl
  startsWithCons x xs e cs | Nothing = Refl

total
searchNodeFlag : (x : Sym) -> (xs : Word) -> (e1, e2 : Bool) -> (cs : List (Sym, Trie)) ->
  search (x :: xs) (Node e1 cs) = search (x :: xs) (Node e2 cs)
searchNodeFlag x xs e1 e2 cs with (childLookup x cs)
  searchNodeFlag x xs e1 e2 cs | Just t  = Refl
  searchNodeFlag x xs e1 e2 cs | Nothing = Refl

total
startsWithNodeFlag : (x : Sym) -> (xs : Word) -> (e1, e2 : Bool) -> (cs : List (Sym, Trie)) ->
  startsWith (x :: xs) (Node e1 cs) = startsWith (x :: xs) (Node e2 cs)
startsWithNodeFlag x xs e1 e2 cs with (childLookup x cs)
  startsWithNodeFlag x xs e1 e2 cs | Just t  = Refl
  startsWithNodeFlag x xs e1 e2 cs | Nothing = Refl

total
insertNil : (e : Bool) -> (cs : List (Sym, Trie)) -> insert [] (Node e cs) = Node True cs
insertNil e cs = Refl

total
insertCons : (b : Sym) -> (vs : Word) -> (e : Bool) -> (cs : List (Sym, Trie)) ->
  insert (b :: vs) (Node e cs) = Node e (childInsert b vs cs)
insertCons b vs e cs = Refl

total
startsWithEmpty : (p : Word) -> startsWith p (Node False []) = isNil p
startsWithEmpty []        = Refl
startsWithEmpty (x :: xs) = Refl

------------------------------------------------------------------------
-- assoc-list lookup hit / miss under childInsert
------------------------------------------------------------------------

total
lookupChildInsertSelf : (b : Sym) -> (vs : Word) -> (cs : List (Sym, Trie)) ->
  childLookup b (childInsert b vs cs) = Just (insert vs (lookupD b cs))
lookupChildInsertSelf b vs [] = rewrite symEqRefl b in Refl
lookupChildInsertSelf b vs ((y, t) :: r) = case boolCase (symEq b y) of
  Left prf =>
    rewrite childInsertConsEq b y vs t r prf in
    rewrite childLookupConsEq b y (insert vs t) r prf in
    rewrite lookupDJust b ((y, t) :: r) t (childLookupConsEq b y t r prf) in Refl
  Right prf =>
    rewrite childInsertConsNeq b y vs t r prf in
    rewrite childLookupConsNeq b y t (childInsert b vs r) prf in
    rewrite lookupDCong b ((y, t) :: r) r (childLookupConsNeq b y t r prf) in
    lookupChildInsertSelf b vs r

total
lookupChildInsertOther : (a, b : Sym) -> (vs : Word) -> (cs : List (Sym, Trie)) ->
  symEq a b = False ->
  childLookup a (childInsert b vs cs) = childLookup a cs
lookupChildInsertOther a b vs [] hab =
  rewrite childLookupConsNeq a b (insert vs emptyTrie) [] hab in Refl
lookupChildInsertOther a b vs ((y, t) :: r) hab = case boolCase (symEq b y) of
  Left prf =>
    let hay : (symEq a y = False)
            = replace {p = \z => symEq a z = False} (symEqSound b y prf) hab in
    rewrite childInsertConsEq b y vs t r prf in
    rewrite childLookupConsNeq a y (insert vs t) r hay in
    rewrite childLookupConsNeq a y t r hay in Refl
  Right prf =>
    rewrite childInsertConsNeq b y vs t r prf in
    case boolCase (symEq a y) of
      Left pay =>
        rewrite childLookupConsEq a y t (childInsert b vs r) pay in
        rewrite childLookupConsEq a y t r pay in Refl
      Right pay =>
        rewrite childLookupConsNeq a y t (childInsert b vs r) pay in
        rewrite childLookupConsNeq a y t r pay in
        lookupChildInsertOther a b vs r hab

------------------------------------------------------------------------
-- search over lookupD (search of the missing child is False)
------------------------------------------------------------------------

total
searchLookupD : (ws : Word) -> (x : Sym) -> (cs : List (Sym, Trie)) ->
  search ws (lookupD x cs) = searchStepM ws (childLookup x cs)
searchLookupD ws x cs with (childLookup x cs)
  searchLookupD ws x cs | Just t  = Refl
  searchLookupD ws x cs | Nothing = searchEmpty ws

total
startsWithLookupDOnNil : (ps : Word) -> (x : Sym) -> (cs : List (Sym, Trie)) ->
  (vs : Word) ->
  (startsWith ps (lookupD x cs) || isPrefix ps vs)
    = (startsWithStepM ps (childLookup x cs) || isPrefix ps vs)
startsWithLookupDOnNil ps x cs vs with (childLookup x cs)
  startsWithLookupDOnNil ps x cs vs | Just t  = Refl
  startsWithLookupDOnNil ps x cs vs | Nothing =
    rewrite startsWithEmpty ps in nilOrPrefix ps vs

------------------------------------------------------------------------
-- refinement of search / startsWith over a single insert
------------------------------------------------------------------------

-- expose the insert-then-(search/startsWith) of a nonempty key as a lookup step
total
searchExposeIns : (a : Sym) -> (ws : Word) -> (b : Sym) -> (vs : Word) ->
  (e : Bool) -> (cs : List (Sym, Trie)) ->
  search (a :: ws) (insert (b :: vs) (Node e cs))
    = searchStepM ws (childLookup a (childInsert b vs cs))
searchExposeIns a ws b vs e cs = searchCons a ws e (childInsert b vs cs)

total
startsWithExposeIns : (a : Sym) -> (ps : Word) -> (b : Sym) -> (vs : Word) ->
  (e : Bool) -> (cs : List (Sym, Trie)) ->
  startsWith (a :: ps) (insert (b :: vs) (Node e cs))
    = startsWithStepM ps (childLookup a (childInsert b vs cs))
startsWithExposeIns a ps b vs e cs = startsWithCons a ps e (childInsert b vs cs)

total
searchInsert : (w, v : Word) -> (t : Trie) ->
  search w (insert v t) = (wordEq w v || search w t)
searchInsert []        []        (Node e cs) = Refl
searchInsert []        (b :: vs) (Node e cs) = Refl
searchInsert (a :: ws) []        (Node e cs) =
  trans (cong (search (a :: ws)) (insertNil e cs)) (searchNodeFlag a ws True e cs)
searchInsert (a :: ws) (b :: vs) (Node e cs) =
  rewrite searchExposeIns a ws b vs e cs in
  rewrite searchCons a ws e cs in
  case boolCase (symEq a b) of
    Left pab =>
      rewrite symEqSound a b pab in
      rewrite lookupChildInsertSelf b vs cs in
      rewrite searchInsert ws vs (lookupD b cs) in
      rewrite wordEqConsEq b b ws vs (symEqRefl b) in
      rewrite searchLookupD ws b cs in Refl
    Right pab =>
      rewrite lookupChildInsertOther a b vs cs pab in
      rewrite wordEqConsNeq a b ws vs pab in Refl

total
startsWithInsert : (p, v : Word) -> (t : Trie) ->
  startsWith p (insert v t) = (startsWith p t || isPrefix p v)
startsWithInsert []        v         t          = Refl
startsWithInsert (a :: ps) []        (Node e cs) =
  rewrite orFalse (startsWith (a :: ps) (Node e cs)) in
  trans (cong (startsWith (a :: ps)) (insertNil e cs)) (startsWithNodeFlag a ps True e cs)
startsWithInsert (a :: ps) (b :: vs) (Node e cs) =
  rewrite startsWithExposeIns a ps b vs e cs in
  rewrite startsWithCons a ps e cs in
  case boolCase (symEq a b) of
    Left pab =>
      rewrite symEqSound a b pab in
      rewrite lookupChildInsertSelf b vs cs in
      rewrite startsWithInsert ps vs (lookupD b cs) in
      rewrite isPrefixConsEq b b ps vs (symEqRefl b) in
      startsWithLookupDOnNil ps b cs vs
    Right pab =>
      rewrite lookupChildInsertOther a b vs cs pab in
      rewrite isPrefixConsNeq a b ps vs pab in
      rewrite orFalse (startsWithStepM ps (childLookup a cs)) in Refl

------------------------------------------------------------------------
-- the two refinement theorems (ported from solution.lean)
------------------------------------------------------------------------

total
searchBuild : (w : Word) -> (ws : List Word) -> search w (build ws) = elemW w ws
searchBuild w []        = searchEmpty w
searchBuild w (v :: vs) =
  rewrite searchInsert w v (build vs) in
  rewrite searchBuild w vs in Refl

total
startsWithBuild : (p : Word) -> (ws : List Word) ->
  startsWith p (build ws) = (isNil p || anyPrefix p ws)
startsWithBuild p []        =
  rewrite startsWithEmpty p in rewrite orFalse (isNil p) in Refl
startsWithBuild p (v :: vs) =
  rewrite startsWithInsert p v (build vs) in
  rewrite startsWithBuild p vs in
  orRot (isNil p) (anyPrefix p vs) (isPrefix p v)

------------------------------------------------------------------------
-- worked example: build ["apple"]
------------------------------------------------------------------------

apple : Word
apple = [A, P, P, L, E]

app : Word
app = [A, P, P]

bb : Word
bb = [B]

main : IO ()
main = do
  let t = build [apple]
  printLn (search apple t)        -- True
  printLn (search app t)          -- False
  printLn (startsWith app t)      -- True
  printLn (startsWith bb t)       -- False
