def letters : Char → List Char
  | '2' => ['a','b','c']
  | '3' => ['d','e','f']
  | '4' => ['g','h','i']
  | '5' => ['j','k','l']
  | '6' => ['m','n','o']
  | '7' => ['p','q','r','s']
  | '8' => ['t','u','v']
  | '9' => ['w','x','y','z']
  | _   => []

def step (d : Char) (acc : List (List Char)) : List (List Char) :=
  (letters d).flatMap (fun l => acc.map (fun cs => l :: cs))

def letterCombinations (s : String) : List String :=
  match s.toList with
  | []  => []
  | cs  => (cs.foldr step [[]]).map (fun l => String.ofList l)

#guard letterCombinations "23" ==
  ["ad","ae","af","bd","be","bf","cd","ce","cf"]
#guard letterCombinations "" == []
#guard (letterCombinations "23").length == 9
