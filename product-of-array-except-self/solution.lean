-- Product of array except self.
-- prefix products times suffix products gives, at each position, the product of
-- everything to the left times everything to the right (i.e. all but self).

-- running products: prefixProds acc xs = [acc, acc*x0, acc*x0*x1, ...]
def prefixProds (acc : Int) : List Int → List Int
  | []      => []
  | y :: ys => acc :: prefixProds (acc * y) ys

def productExceptSelf (xs : List Int) : List Int :=
  let pre := prefixProds 1 xs
  let suf := (prefixProds 1 xs.reverse).reverse
  (pre.zip suf).map (fun (a, b) => a * b)

#guard productExceptSelf [1, 2, 3, 4] == [24, 12, 8, 6]
#guard productExceptSelf [5, 2, 3] == [6, 15, 10]
