-- Hash complement lookup via an assoc list of value -> index seen so far.
def twoSum (nums : List Int) (target : Int) : Option (Nat × Nat) :=
  let rec go (i : Nat) (seen : List (Int × Nat)) : List Int → Option (Nat × Nat)
    | [] => none
    | x :: xs =>
      match seen.find? (fun p => p.fst == target - x) with
      | some (_, j) => some (j, i)
      | none        => go (i + 1) ((x, i) :: seen) xs
  go 0 [] nums

#guard twoSum [2,7,11,15] 9 == some (0, 1)
#guard twoSum [3,2,4] 6     == some (1, 2)
