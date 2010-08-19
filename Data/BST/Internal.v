Require Import OrderedType.
Require Import BinNat.
Open Scope N_scope.

Module ordmap (X: OrderedType).

Definition Size := N.

(* strictness ignored because Coq has no fixed evaluation strategy *)

Section map.

Variable a: Type.
Definition k := X.t.


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
    Bin (1 + size l + size r) key x l r.

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

Definition lmap: Map -> nat :=
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
  near (lmap a) (lmap b).

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
clear H0 H ne IHm a.
rewrite mn.
reflexivity.
intro msn.
clear H0 H ne IHm a.
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


Definition empty : Map := Tip.

Definition singleton : k -> a -> Map :=
  fun key x =>
    Bin (Npos xH) key x Tip Tip.

Definition assert_false := bin.

Require Import Compare_dec.

Definition doubleL : k -> a -> Map -> Map -> Map :=
  fun k1 x1 t1 t2 =>
    match t2 with
      | Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4 =>
        bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
      | _ => assert_false k1 x1 t1 t2 
    end.

Definition singleL : k -> a -> Map -> Map -> Map :=
  fun k1 x1 t1 t2 =>
    match t2 with
      | (Bin _ k2 x2 t2 t3)  => bin k2 x2 (bin k1 x1 t1 t2) t3
      | _ => assert_false k1 x1 t1 t2
    end.

Definition rotateL : k -> a -> Map -> Map -> Map :=
  fun k x l r =>
    match r with
      | Tip => assert_false k x l r
      | Bin _ _ _ rl rr =>
        match nat_compare (lmap rr) (lmap rl) with
          | Lt => doubleL k x l r
          | _ => singleL k x l r
        end
    end.

Definition balanceGT: k -> a -> Map -> Map -> Map :=
  fun k x l r =>
    match (isBalanced l r) with
      | true => bin k x l r
      | false => rotateL k x l r
    end.

Definition doubleR : k -> a -> Map -> Map -> Map :=
  fun k1 x1 t t4 =>
    match t with
      | (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) =>
        bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
      | _ => assert_false k1 x1 t t4
    end.

Definition singleR : k -> a -> Map -> Map -> Map :=
  fun k1 x1 t t3 =>
    match t with
      | (Bin _ k2 x2 t1 t2) => bin k2 x2 t1 (bin k1 x1 t2 t3)
      | _ => assert_false k1 x1 t t3
    end.

Definition rotateR : k -> a -> Map -> Map -> Map :=
  fun k x l r =>
    match l with
      | Bin _ _ _ ll lr =>
        match (nat_compare (lmap ll) (lmap lr)) with
          | Lt => doubleR k x l r
          | _ => singleR k x l r
        end
      | _ => assert_false k x l r
    end.

Definition balanceLT (kx: k) (x:a) (l r:Map) :=
  match (isBalanced r l) with
    | true => bin kx x l r
    | false => rotateR kx x l r
  end.
        
Fixpoint insert (kx:k) (x:a) (t:Map): Map :=
  match t with
    | Tip => singleton kx x
    | Bin _ ky y l r =>
      match X.compare kx ky with
        | GT _ => balanceGT ky y l (insert kx x r)
        | LT _ => balanceLT ky y (insert kx x l) r
        | EQ _ => bin kx x l r
      end
  end.

Open Scope bool_scope.

Fixpoint balanced (t: Map) : bool :=
  match t with
    | Tip => true
    | Bin _ _ _ l r =>
      isBalanced l r && isBalanced r l && balanced l && balanced r
  end.

Definition less_than (x y: k): bool :=
  match X.compare x y with
    | LT _ => true
    | _ => false
  end.

Definition more_than (x y: k): bool :=
  match X.compare x y with
    | GT _ => true
    | _ => false
  end.

Fixpoint bounded (lo hi: k -> bool) (t: Map): bool :=
  match t with
    | Tip => true
    | Bin _ kx _ l r =>
      (lo kx) && (hi kx) && bounded lo (less_than kx) l && bounded (more_than kx) hi r
  end.

Definition const (p q: Type): p -> q -> p.
intros p q.
intros x y.
exact x.
Defined.

Definition ordered : Map -> bool :=
  fun t =>
    bounded (const bool k true) (const bool k true) t.

Require Import OrderedTypeEx.

Fixpoint realsize (t: Map) :=
  match t with
    | Tip => N0
    | Bin sz _ _ l r =>
      1 + (realsize l) + (realsize r)
  end.

Definition Nequal (x y: N): bool :=
  match Ncompare x y with
    | Eq => true
    | _ => false
  end.

Import Bool.

Lemma Nequal_equal:
  forall (x y: N),
    Is_true (Nequal x y) -> x = y.
  intros x y.
  compute [Is_true Nequal].
  intro neq.
  apply Ncompare_Eq_eq.
  generalize neq.
  clear neq.
  set (b := x ?= y).
  case b.
  auto.
  apply False_ind.
  apply False_ind.
  Qed.
  
      
Definition validsize : Map -> bool :=
  fun t => Nequal (realsize t) (size t).

Definition valid (t:Map) :=
  balanced t && ordered t && validsize t.


Lemma validsize_tip:
  Is_true (validsize Tip).
  simpl.
  trivial.
Qed.

Lemma Ncompare_eq_comp : forall n m:N, n = m -> (n ?= m) = Eq.
  intros n m.
  generalize Ncompare_eq_correct.
  intro cor.
  elim cor with n m.
  clear cor.
  intros sound comp.
  clear sound.
  exact comp.
  Qed.

Lemma equal_Nequal:
  forall (x y: N),
    x = y -> Is_true (Nequal x y).
  intros x y.
  intro pre.
  compute [Is_true Nequal].
  assert ((x ?= y) = Eq).
  apply Ncompare_eq_comp.
  exact pre.
  rewrite H.
  trivial.
  Qed.
  



Lemma validsize_bin:
  forall (kx : k) (x : a) (l r : Map),
    Is_true (validsize l) -> Is_true (validsize r) ->
    Is_true (validsize (bin kx x l r)).
  intros kx x l r.
  compute [validsize].
  intros lvalid rvalid.
  assert (realsize (bin kx x l r) = size (bin kx x l r)).
  compute [bin].
  assert (realsize l = size l).
  clear rvalid r.
  apply Nequal_equal.
  exact lvalid.
  assert (realsize r = size r).
  clear lvalid l H.
  apply Nequal_equal.
  exact rvalid.
  rewrite <- H.
  rewrite <- H0.
  compute [size realsize].
  reflexivity.
  apply equal_Nequal.
  exact H.
  Qed.

  (* this is wrong
Lemma validsize_bin_inner:
  forall (s : Size) (k0 : k) (a0 : a) (l1 : Map) (l2 : Map), 
    Is_true (validsize (Bin s k0 a0 l1 l2)) -> Is_true (validsize l1).
*)

(* this is OK. but can we use this later ?
Lemma validsize_singleR:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize l) ->
    Is_true (validsize r) ->
    l <> Tip ->
    Is_true (validsize (singleR kx x l r)).
  intros kx x l.
  generalize kx x.
  clear kx x.
  destruct l.
  intros kx x r.
  intros lvalid rvalid.
  intro nontip.
  apply False_ind.
  apply nontip.
  reflexivity.
  intros kx x r.
  compute [singleR].
  compute [validsize].
  intros one two.
  intro irr.
  clear irr.
  apply equal_Nequal.
  apply Nequal_equal in one.
  apply Nequal_equal in two.
  assert (size (bin k0 a0 l1 (bin kx x l2 r)) = 1 + size l1 +
    (1 + size l2 + size r)).
  compute [size].
  reflexivity.
  rewrite H.
  clear H.
  rewrite <- two.
  clear two.
  assert (realsize (bin k0 a0 l1 (bin kx x l2 r)) =
    1 + realsize l1 + 1 + realsize l2 + realsize r).
  compute [bin].
  compute [realsize].
  compute.
  reflexivity.
*)  
  
  (* this is wrong: apply validsize_bin. *)
  
  
(* this is OK, but can we use thie? 
Lemma validsize_balanceLT:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize l) ->
    Is_true (validsize r) ->
    Is_true (validsize (balanceLT kx x l r)).
  intros kx x l r.
  intros vl vr.
  compute [balanceLT].
  case (isBalanced r l).
  generalize vl vr.
  compute [validsize bin].
  clear vl vr.
  intros vl vr.
  assert (realsize l = size l).
  clear r vr.
  generalize vl.
  clear vl.
  apply Nequal_equal.
  compute [Is_true Nequal].
  assert ((realsize (Bin (1 + size l + size r) kx x l r)
        ?= size (Bin (1 + size l + size r) kx x l r)) = Eq).
  apply Ncompare_eq_comp.
  rewrite <- H.
  assert (realsize r = size r).
  clear l vl H.
  generalize vr.
  clear vr.
  compute [Is_true Nequal].
  intro vr.
  apply Ncompare_Eq_eq.
  generalize vr.
  clear vr.
  set (b := realsize r ?= size r).
  case b.
  auto.
  apply False_ind.
  apply False_ind.
  rewrite <- H0.
  clear H H0.
  compute [Is_true].
  assert (realsize (Bin (1 + realsize l + realsize r) kx x l r) =
  1 + realsize l + realsize r).
  compute [realsize].
  reflexivity.
  rewrite H.
  clear H.
  assert (1 + realsize l + realsize r = (size (Bin (1 + realsize l + realsize r) kx x l r))).
  compute [size].
  reflexivity.
  rewrite <- H.
  compute [Nequal].
  assert ((1 + realsize l + realsize r ?= 1 + realsize l + realsize r) = Eq).
  apply Ncompare_eq_comp.
  reflexivity.
  reflexivity.
  rewrite H0.
  auto. *)
  (* Is_true (validsize (rotateR kx x l r)) *)
  

Lemma validsize_ind:
  forall (t: Map) (kx: k) (x: a),
    Is_true (validsize t) ->
    Is_true (validsize (insert kx x t)).
intros t kx x.
induction t.
intro pre.
simpl.
trivial.
intro pre.
simpl.
case (X.compare kx k0).
intro irr.
clear irr.
(*   Is_true (validsize (balanceLT k0 a0 (insert kx x t1) t2)) *)



End map.



Extraction Language Haskell.
Recursive Extraction insert.