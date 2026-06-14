module Main

closeOf : Char -> Char
closeOf '(' = ')'
closeOf '[' = ']'
closeOf '{' = '}'
closeOf c   = c

isOpen : Char -> Bool
isOpen c = c == '(' || c == '[' || c == '{'

-- Push the expected closer for each opener; match closers against the stack top.
isValid : String -> Bool
isValid s = go (unpack s) []
  where
    go : List Char -> List Char -> Bool
    go [] []        = True
    go [] (_ :: _)  = False
    go (c :: cs) stack =
      if isOpen c
        then go cs (closeOf c :: stack)
        else case stack of
               (top :: rest) => if c == top then go cs rest else False
               []            => False

------------------------------------------------------------------------
-- Verification (single bracket type). `check` is `isValid` specialised to
-- one kind of bracket, with the stack-of-closers collapsed into a depth
-- counter. Working over a two-constructor `Paren` type (not the primitive
-- `Char`) keeps every proof total. We show `check 0` accepts EXACTLY the
-- balanced strings.
------------------------------------------------------------------------

data Paren = LP | RP   -- '(' and ')'

-- `Bal d xs`: reading xs from depth d returns to 0 without going negative;
-- `Bal 0` is "well-formed parentheses" (the relation proven for generate-parenthesis).
data Bal : Nat -> List Paren -> Type where
  BNil   : Bal Z []
  BOpen  : Bal (S d) xs -> Bal d (LP :: xs)
  BClose : Bal d xs -> Bal (S d) (RP :: xs)

check : Nat -> List Paren -> Bool
check d []         = case d of { Z => True; S _ => False }
check d (LP :: cs) = check (S d) cs
check d (RP :: cs) = case d of { Z => False; S d' => check d' cs }

-- every balanced string is accepted
checkComplete : (cs : List Paren) -> {d : Nat} -> Bal d cs -> check d cs = True
checkComplete []         BNil       = Refl
checkComplete (LP :: cs) (BOpen p)  = checkComplete cs p
checkComplete (RP :: cs) (BClose p) = checkComplete cs p

-- every accepted string is balanced (total: `Paren` has no non-bracket case)
checkSound : (d : Nat) -> (cs : List Paren) -> check d cs = True -> Bal d cs
checkSound d []         prf = case d of
                               Z   => BNil
                               S _ => absurd prf
checkSound d (LP :: cs) prf = BOpen (checkSound (S d) cs prf)
checkSound d (RP :: cs) prf = case d of
                               Z   => absurd prf
                               S k => BClose (checkSound k cs prf)

-- the two directions: `check 0` accepts a string iff it is balanced (Bal 0)
acceptsBalanced : (cs : List Paren) -> Bal 0 cs -> check 0 cs = True
acceptsBalanced cs = checkComplete cs

onlyBalanced : (cs : List Paren) -> check 0 cs = True -> Bal 0 cs
onlyBalanced = checkSound 0

main : IO ()
main = do
  printLn (isValid "()[]{}")          -- True  (multi-bracket LeetCode solution)
  printLn (isValid "(]")              -- False
  printLn (isValid "{[]}")            -- True
  -- verified single-bracket recognizer (check 0 agrees with isValid on '(' ')')
  printLn (check 0 [LP, LP, RP, RP])  -- True   "(())"
  printLn (check 0 [LP, LP, RP])      -- False  "(()"
  printLn (check 0 [RP, LP])          -- False  ")("
