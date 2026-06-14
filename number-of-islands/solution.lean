/-
LeetCode 200: Number of Islands
Flood-fill DFS over a grid of '1' (land) / '0' (water).
Grid is a List String (rows). Out-of-bounds reads as water.
Lean 4, core library only.
-/

abbrev Grid := List (List Char)

def nth : Nat → List α → Option α
  | _,     []      => none
  | 0,     x :: _  => some x
  | n + 1, _ :: xs => nth n xs

-- grid lookup with Int coords; out of bounds reads as water
def cellAt (grid : Grid) (r c : Int) : Char :=
  if r < 0 || c < 0 then '0'
  else match nth r.toNat grid with
       | none     => '0'
       | some row => (nth c.toNat row).getD '0'

def posEq (a b : Int × Int) : Bool := a.1 == b.1 && a.2 == b.2

def memPos (p : Int × Int) (xs : List (Int × Int)) : Bool :=
  xs.any (posEq p)

-- flood fill: add every connected land cell to the visited set
partial def flood (grid : Grid) (pos : Int × Int)
    (visited : List (Int × Int)) : List (Int × Int) :=
  if cellAt grid pos.1 pos.2 != '1' || memPos pos visited then
    visited
  else
    let (r, c) := pos
    let v1 := pos :: visited
    let v2 := flood grid (r + 1, c) v1
    let v3 := flood grid (r - 1, c) v2
    let v4 := flood grid (r, c + 1) v3
    flood grid (r, c - 1) v4

def numIslands (grid : Grid) : Nat :=
  let rows : Int := grid.length
  let cols : Int := (grid.head?.map List.length).getD 0
  let coords : List (Int × Int) :=
    (List.range rows.toNat).flatMap (fun r =>
      (List.range cols.toNat).map (fun c => (((r : Nat) : Int), ((c : Nat) : Int))))
  let step : (List (Int × Int) × Nat) → (Int × Int) → (List (Int × Int) × Nat) :=
    fun (visited, count) pos =>
      if cellAt grid pos.1 pos.2 == '1' && !memPos pos visited then
        (flood grid pos visited, count + 1)
      else
        (visited, count)
  (coords.foldl step ([], 0)).2

def mkGrid (rows : List String) : Grid := rows.map String.toList

#guard numIslands (mkGrid ["11110", "11010", "11000", "00000"]) == 1
#guard numIslands (mkGrid ["11000", "11000", "00100", "00011"]) == 3
