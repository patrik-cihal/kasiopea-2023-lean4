import «Kasiopea2023Lean»
import Lean

def readNatLine (stream : IO.FS.Stream) := do
  let n_raw ← stream.getLine
  return n_raw.trim.toNat!

def readNatsLine (stream: IO.FS.Stream) : IO (List Nat) := do
  let line ← stream.getLine
  return line.trim.splitOn.map (fun (str : String) => str.toNat!)

def rozcvicka (w : Nat) (h : Nat) (so : Nat) (no : Nat) : Nat :=
  min (so + no + h) ((w-so) + w-no + h)

def rozcvicka_io : IO Unit := do
  let stdin ← IO.getStdin
  let t ← readNatLine stdin
  (List.range t).forM (fun _ => do
    let w ← readNatLine stdin
    let h ← readNatLine stdin
    let so ← readNatLine stdin
    let no ← readNatLine stdin
    let ans := rozcvicka w h so no
    IO.println ans
  )


def rekordy (l : List Nat) : Nat :=
  (l.foldl (fun prev cur => if cur > prev.fst then (cur, prev.snd+1) else prev) (0, 0)).snd

def rekordy_io : IO Unit := do
  let stdin ← IO.getStdin
  let t ← readNatLine stdin
  (List.range t).forM (fun _ => do
    let n ← readNatLine stdin
    let a ← readNatsLine stdin
    let ans := rekordy a
    IO.println s!"{ans}"
  )


def ponozky_pairs (l : List (Nat × Nat)) : Nat :=
  let ls := Lean.RBTree.fromList l (fun x y => compare x.fst y.fst)
  (ls.fold (fun (prev, dist) (x1, x2) => (x2, dist + (if prev > x1 then (prev-x1) else (x1-prev)) + x2-x1)) (0, 0)).snd

def ponozky (l : List Nat) : IO Nat := do
  let lp := (l.enum.foldl (fun (l, hm) (i, cur) => match (hm[cur]) with
    | some prev => ((prev, i)::l, hm)
    | none => (l, hm.insert cur i)
    ) ([], Lean.mkHashMap))
  return ponozky_pairs lp.fst


-- This one requires a lot of stack size, I used 1 GB
def ponozky_io : IO Unit := do
  let stdin ← IO.getStdin
  let t ← readNatLine stdin
  (List.range t).forM (fun i => do
    let _←readNatLine stdin
    let a ← readNatsLine stdin
    let ans ← ponozky a
    IO.println ans
  )

partial def deepestNodeHelper (cur : Nat) (tree : Array (List (Nat × Nat))) (par : Nat) : Nat × Nat :=
  (tree[cur]!.filter (fun x => x.fst != par)).foldl (fun (mxd, prev) (v, dis) => let res := deepestNodeHelper v tree cur; let res := (res.fst + dis, res.snd); if res.fst > mxd then res else (mxd, prev)) (0, cur)

def deepestNode (root : Nat) (tree : Array (List (Nat×Nat))) : Nat × Nat :=
  deepestNodeHelper root tree root

def d (tree : Array (List (Nat × Nat))) :=
  let diameter : Nat := (deepestNode (deepestNode 0 tree).snd tree).fst
  let tree_span := (tree.map (fun ch => (ch.map (fun (_, l) => l)).foldl Nat.add 0)).foldl Nat.add 0
  let edges_traversed := tree_span - diameter
  edges_traversed

def d_io : IO Unit := do
  let stdin ← IO.getStdin
  let t ← readNatLine stdin
  (List.range t).forM (fun _ => do
    let n ← readNatLine stdin
    let tree ← (List.range (n-1)).foldlM (fun t _ => do
      let line ← (readNatsLine stdin)
      let u := line[0]!-1
      let v := line[1]!-1
      let l := line[2]!
      return ((t.set! (u) ((v, l)::t[u]!)).set! (v) ((u, l)::t[v]!))
      ) ((List.range n).map (fun _ => [])).toArray
    let ans := d tree
    IO.println ans
  )



def main : IO Unit :=
  d_io
