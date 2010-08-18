Require Import OrderedType.

Definition Size := nat.

(* strictness ignored because Coq has no fixed evaluation strategy *)

Section map.

  Variable k a: Set.

Inductive Map :=
| Tip: Map
| Bin: Size -> k -> a -> Map -> Map -> Map.

Fixpoint size (m: Map): Size :=
  match m with
    | Tip => 0
    | Bin sz _ _ _ _ => sz
  end.

Definition bin: k -> a -> Map -> Map -> Map := 
  fun key x l r =>
    Bin (size l + size r + 1) key x l r.

Definition .<.: Size -> Size -> Bool.

(* and operation ! *)
 