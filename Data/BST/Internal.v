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
      
Definition lN: N -> nat :=
  fun n =>
    match n with
      | N0 => O
      | Npos p => lpos p
    end.

Definition l: Map -> nat :=
  fun t => lN (size t).

Fixpoint near (m n: nat): bool :=
  match m, n with
    | O, O => true
    | O, S O => true
    | S O, O => true
    | S m', S n' => near m' n'
    | _, _ => false
  end.

Lemma near_ind:
  forall (m n: nat),
    near (S m) (S n) = near m n.
  intros m n.
  simpl.
  case m.
  reflexivity.
  intro n0.
  reflexivity.
  Qed.

Definition isBalanced: Map -> Map -> bool :=
fun a b =>
  near (l a) (l b).

Lemma near_plus_minus:
  forall m n: nat,
    near m n = true <-> m = n \/ m = S n \/ n = S m.
induction m.
intro n.
split.
compute.
case n.
intro irr.
left.
reflexivity.
clear n.
intro n.
case n.
intro irr.
right.
right.
reflexivity.
intro n0.
intro f.
discriminate.
compute.
case n.
intro.
reflexivity.
clear n.
intro n.
intro disj.
case disj.
intro f.
discriminate.
clear disj.
intro disj.
case disj.
intro f.
discriminate.
intro f.
assert (n=O).
injection f.
auto.
rewrite H.
reflexivity.
intro n.
case n.
split.
compute.
case m.
intro irr.
right.
left.
reflexivity.
clear n.
intro n.
intro.
discriminate.
compute.
case m.
intro.
reflexivity.
clear n.
intro n.
intro disj.
elim disj.
intro f.
discriminate.
clear disj.
intro disj.
case disj.
clear disj.
intro f.
discriminate.
intro f.
discriminate.
clear n.
intro n.
split.
intro ne.
assert (near m n = true).
inversion ne.
case m.
reflexivity.
intro n0.
reflexivity.
assert (m = n \/ m = (S n) \/ n = (S m)).
elim IHm with n.
clear IHm.
intro ih.
intro irr.
clear irr.
apply ih.
exact H.
case H0.
intro mn.
left.
clear H0 H ne IHm a k.
rewrite mn.
reflexivity.
intro msn.
clear H0 H ne IHm a k.
case msn.
clear msn.
intro msn.
right.
left.
rewrite msn.
reflexivity.
intro nsm.
right.
right.
rewrite nsm.
reflexivity.
intro disj.
elim disj.
clear disj.
intro smsn.
assert (near m n = true).
elim IHm with n.
intro irr.
intro ih.
apply ih.
left.
injection smsn.
auto.
rewrite near_ind.
exact H.
clear disj.
intro disj.
case disj.
clear disj.
intro smssn.
rewrite near_ind.
assert (m=S n).
injection smssn.
auto.
elim IHm with n.
intro irr.
clear irr.
intro app.
apply app.
right.
left.
exact H.
clear disj.
intro snssm.
assert (n = S m).
injection snssm.
auto.
rewrite near_ind.
elim IHm with n.
intro irr.
clear irr.
intro app.
apply app.
right.
right.
exact H.
Qed.


