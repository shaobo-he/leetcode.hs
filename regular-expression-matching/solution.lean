-- Brzozowski derivatives (port of the Idris/Haskell solution).

inductive RE where
  | empty : RE
  | eps   : RE
  | dot   : RE
  | chr   : Char → RE
  | seq   : RE → RE → RE
  | alt   : RE → RE → RE
  | star  : RE → RE
  deriving Inhabited

open RE

def nullable : RE → Bool
  | empty   => false
  | eps     => true
  | dot     => false
  | chr _   => false
  | seq a b => nullable a && nullable b
  | alt a b => nullable a || nullable b
  | star _  => true

def deriv (c : Char) : RE → RE
  | empty   => empty
  | eps     => empty
  | dot     => eps
  | chr c'  => if c == c' then eps else empty
  | alt a b => alt (deriv c a) (deriv c b)
  | seq a b =>
      if nullable a then alt (seq (deriv c a) b) (deriv c b)
      else seq (deriv c a) b
  | star a  => seq (deriv c a) (star a)

def matchRE (s : String) (r : RE) : Bool :=
  nullable (s.toList.foldl (fun acc c => deriv c acc) r)

def atom (c : Char) : RE :=
  if c == '.' then dot else chr c

-- structural: every recursive call is on a proper suffix of the input
def parse : List Char → RE
  | []             => eps
  | c :: '*' :: rest => seq (star (atom c)) (parse rest)
  | c :: rest      => seq (atom c) (parse rest)
  termination_by l => l.length
  decreasing_by all_goals simp_wf; all_goals omega

def isMatch (s p : String) : Bool :=
  matchRE s (parse p.toList)

#guard isMatch "aa" "a" == false
#guard isMatch "aa" "a*" == true
#guard isMatch "ab" ".*" == true
#guard isMatch "aab" "c*a*b" == true
#guard isMatch "mississippi" "mis*is*p*." == false
