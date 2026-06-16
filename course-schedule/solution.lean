-- Topological sort via DFS. graph as assoc list: prereq -> dependents.

def addEdge (k v : Int) : List (Int × List Int) → List (Int × List Int)
  | [] => [(k, [v])]
  | (k', vs) :: rest =>
    if k == k' then (k', v :: vs) :: rest else (k', vs) :: addEdge k v rest

def buildGraph (prereqs : List (List Int)) : List (Int × List Int) :=
  prereqs.foldl (fun g e =>
    match e with
    | a :: b :: _ => addEdge b a g
    | _           => g) []

def succsOf (node : Int) (g : List (Int × List Int)) : List Int :=
  match g.find? (fun p => p.fst == node) with
  | some (_, vs) => vs
  | none         => []

-- DFS with status map (1 = on stack, 2 = finished); none signals a cycle.
-- Left `partial`: the recursion explores arbitrary graph successors (not
-- structure), so a total version is a verified-DFS proof — a finite "unvisited
-- nodes" measure over the graph's node set, with the status map's monotonicity
-- carried through the return type.  Future work.
partial def visit (g : List (Int × List Int))
    (acc : Option (List (Int × Int) × List Int)) (node : Int) :
    Option (List (Int × Int) × List Int) :=
  match acc with
  | none => none
  | some (st, order) =>
    match st.find? (fun p => p.fst == node) with
    | some (_, 2) => some (st, order)
    | some _      => none
    | none =>
      match (succsOf node g).foldl (visit g) (some ((node, 1) :: st, order)) with
      | none               => none
      | some (st2, order2) => some ((node, 2) :: st2, node :: order2)

def findOrder (n : Nat) (prereqs : List (List Int)) : List Int :=
  let g     := buildGraph prereqs
  let nodes := (List.range n).map (fun i => (Int.ofNat i))
  match nodes.foldl (visit g) (some ([], [])) with
  | some (_, order) => order
  | none            => []

#guard findOrder 4 [[1,0],[2,0],[3,1],[3,2]] == [0, 1, 2, 3]  -- a valid topological order
#guard findOrder 2 [[1,0],[0,1]]             == []
