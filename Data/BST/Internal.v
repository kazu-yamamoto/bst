Require Import OrderedType.
Require Import BinNat.
Open Scope N_scope.

Definition Size := N.

(* strictness ignored because Coq has no fixed evaluation strategy *)

Section map.

  Variable k a: Set.

Inductive Map :=
| Tip: Map
| Bin: Size -> k -> a -> Map -> Map -> Map.

Fixpoint size (m: Map): Size :=
  match m with
    | Tip => N0
    | Bin sz _ _ _ _ => sz
  end.

Definition bin: k -> a -> Map -> Map -> Map := 
  fun key x l r =>
    Bin (size l + size r + 1) key x l r.

Require Import BinPos.

(*
This fails because induction does not type match.
Possible fix: use (option N).   

Fixpoint b_and_pos (m n: positive): N :=
  match m with
    | xH =>
      match n with
        | xH => Npos xH
        | xI _ => Npos xH
        | xO _ => N0
      end
    | xI m' =>
      match n with
        | xH => xH
        | xI n' => xI (b_and_pos m' n')
        | xO n' => xO (b_and_pos m' n')
      end
    | xO m' =>
      xO (b_and_pos m' (Pdiv2 n))
  end.

*)


(*
This fails because Coq is not convinced this terminates.

Fixpoint b_and (m n: N): N :=
  match m with
    | N0 => N0
    | Npos xH =>
      match n with
        | Npos xH => Npos xH
        | Npos (xI _) => Npos xH
        | Npos (xO _) => N0
        | N0 => N0
      end
    | Npos (xO m') =>
      Ndouble (b_and (Npos m') (Ndiv2 n))
    | Npos (xI m') =>
      match n with
        | Npos xH => Npos xH
        | Npos (xI _) =>
          Nplus (Npos xH) (Ndouble (b_and (Npos m') (Ndiv2 n)))
        | Npos (xO _) =>
          (Ndouble (b_and (Npos m') (Ndiv2 n)))
        | N0 => N0
      end
  end.
*)

Fixpoint lpos (n: positive): nat :=
  match n with
    | xH => S O
    | xO n' => S (lpos n')
    | xI n' => S (lpos n')
  end.
      
Definition l: N -> nat :=
  fun n =>
    match n with
      | N0 => O
      | Npos p => lpos p
    end.


(* and operation ! *)
 