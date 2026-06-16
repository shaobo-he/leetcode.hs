-- Recursive-descent calculator.
-- expr = term (('+'|'-') term)* ; term = factor (('*'|'/') factor)*

def isDigit' (c : Char) : Bool := c >= '0' && c <= '9'

def dropSpaces : List Char → List Char
  | ' ' :: cs => dropSpaces cs
  | cs        => cs

-- integer division truncated toward zero (LeetCode semantics)
def tdiv (a b : Int) : Int :=
  let q : Int := Int.ofNat (a.natAbs / b.natAbs)
  if (a < 0) != (b < 0) then -q else q

def charToDigit (c : Char) : Int := Int.ofNat (c.toNat - '0'.toNat)

def parseNumber (cs : List Char) : Int × List Char :=
  let rec go (acc : Int) : List Char → Int × List Char
    | c :: rest => if isDigit' c then go (acc * 10 + charToDigit c) rest else (acc, c :: rest)
    | []        => (acc, [])
  go 0 cs

-- Recursive-descent expr/term/factor.  Left `partial`: parseExpr→parseTerm→
-- parseFactor recurse on the *same* input and only shrink it when parseFactor
-- consumes a '(' before recursing into parseExpr, so a total version is a
-- verified-parser proof — each parser carrying a length bound on its remainder
-- through its return type, with a lexicographic (length, rank) measure.  Future
-- work.
mutual
  partial def parseExpr (cs : List Char) : Int × List Char :=
    let (t, rest) := parseTerm cs
    exprTail t (dropSpaces rest)

  partial def exprTail (acc : Int) : List Char → Int × List Char
    | '+' :: cs => let (t, rest) := parseTerm cs; exprTail (acc + t) (dropSpaces rest)
    | '-' :: cs => let (t, rest) := parseTerm cs; exprTail (acc - t) (dropSpaces rest)
    | cs        => (acc, cs)

  partial def parseTerm (cs : List Char) : Int × List Char :=
    let (f, rest) := parseFactor cs
    termTail f (dropSpaces rest)

  partial def termTail (acc : Int) : List Char → Int × List Char
    | '*' :: cs => let (f, rest) := parseFactor cs; termTail (acc * f) (dropSpaces rest)
    | '/' :: cs => let (f, rest) := parseFactor cs; termTail (tdiv acc f) (dropSpaces rest)
    | cs        => (acc, cs)

  partial def parseFactor (cs0 : List Char) : Int × List Char :=
    match dropSpaces cs0 with
    | '(' :: rest =>
      let (v, rest1) := parseExpr rest
      match dropSpaces rest1 with
      | ')' :: rest2 => (v, rest2)
      | other        => (v, other)
    | cs => parseNumber cs
end

def calculate (s : String) : Int := (parseExpr s.toList).fst

#guard calculate "1+1"           == 2
#guard calculate "6-4/2"         == 4
#guard calculate "2*(5+5*2)+3*5" == 45
#guard calculate "14-3/2"        == 13
#guard calculate "(0-3)/2"       == -1
