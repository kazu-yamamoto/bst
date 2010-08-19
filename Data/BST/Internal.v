Require Import OrderedType.
Require Import BinNat.
Require Import NArithRing.
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

Fixpoint validsize_rec (t: Map): bool :=
  match t with
    | Tip => validsize t
    | Bin _ _ _ l r =>
      validsize t && validsize_rec l && validsize_rec r
  end.

Definition valid (t:Map) :=
  balanced t && ordered t && validsize_rec t.


Lemma validsize_rec_tip:
  Is_true (validsize_rec Tip).
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


Lemma validsize_rec_hereditary1:
  forall (s: Size) (k0: k) (a0: a) (l1 l2: Map),
    Is_true (validsize_rec (Bin s k0 a0 l1 l2)) ->
    Is_true (validsize_rec l1).
  intros s k0 a0 l1 l2.
  simpl.
  case (validsize_rec l1).
  intro irr.
  simpl.
  auto.
  generalize (
   validsize (Bin s k0 a0 l1 l2)
   ).
  intro b.
  generalize (
    validsize_rec l2
    ).
  intro c.
  compute.
  case b.
  auto.
  auto.
  Qed.
  
Lemma balanced_hereditary1:
  forall (s: Size) (k0: k) (a0: a) (l1 l2: Map),
    Is_true (balanced (Bin s k0 a0 l1 l2)) ->
    Is_true (balanced l1).
  intros s k0 a0 l1 l2.
  simpl.
  case (balanced l1).
  intro irr.
  simpl.
  auto.
  generalize (isBalanced l1 l2) (isBalanced l2 l1) (balanced l2).
  intros b c d.
  case b; case c; case d; auto.
  Qed.
    
Lemma balanced_hereditary2:
  forall (s: Size) (k0: k) (a0: a) (l1 l2: Map),
    Is_true (balanced (Bin s k0 a0 l1 l2)) ->
    Is_true (balanced l2).
  intros s k0 a0 l1 l2.
  simpl.
  case (balanced l2).
  intro irr.
  simpl.
  auto.
  generalize (isBalanced l1 l2) (isBalanced l2 l1) (balanced l1).
  intros b c d.
  case b; case c; case d; auto.
  Qed.
    
Lemma validsize_rec_hereditary2:
  forall (s: Size) (k0: k) (a0: a) (l1 l2: Map),
    Is_true (validsize_rec (Bin s k0 a0 l1 l2)) ->
    Is_true (validsize_rec l2).
  intros s k0 a0 l1 l2.
  simpl.
  case (validsize_rec l2).
  intro irr.
  simpl.
  auto.
  generalize (
   validsize (Bin s k0 a0 l1 l2)
   ).
  intro b.
  generalize (
    validsize_rec l1
    ).
  intro c.
  compute.
  case c.
  case b.
  auto.
  auto.
  case b.
  auto. auto.
  Qed.
  
Lemma validsize_rec_self:
  forall (m: Map),
    Is_true (validsize_rec m) ->
    Is_true (validsize m).
  intro m.
  destruct m.
  intro irr.
  auto.
  simpl.
  generalize (validsize_rec m1) (validsize_rec m2).
  intros b c.
  generalize (validsize (Bin s k0 a0 m1 m2)).
  intro d.
  compute.
  case d.
  auto.
  auto.
  Qed.

Lemma validsize_rec_bin:
  forall (kx : k) (x : a) (l r : Map),
    Is_true (validsize_rec l) -> Is_true (validsize_rec r) ->
    Is_true (validsize_rec (bin kx x l r)).
  intros kx x l r lval rval.
  assert (
    validsize_rec (bin kx x l r) =
    validsize (bin kx x l r) &&
    validsize_rec l && validsize_rec r).
  auto.
  rewrite H.
  clear H.
  generalize lval rval.
  case (validsize_rec l).
  intro irr.
  clear irr.
  case (validsize_rec r).
  intro irr.
  clear irr.
  assert (
    Is_true (validsize (bin kx x l r) && true && true) =
    Is_true (validsize (bin kx x l r))).
  case (validsize (bin kx x l r)).
  auto.
  auto.
  rewrite H.
  clear H.
  apply validsize_bin.
  apply validsize_rec_self.
  exact lval.
  apply validsize_rec_self.
  exact rval.
  intro f.
  absurd (Is_true false).
  auto.
  exact f.
  intro f.
  absurd (Is_true false).
  auto.
  assumption.
  Qed.

Lemma validsize_rec_expand:
  forall (kx: k) (x: a) (l r:Map),
    (validsize_rec (bin kx x l r) = validsize (bin kx x l r) && validsize_rec l && validsize_rec r).
  intros.
  auto.
  Qed.

Lemma validsize_rec_expand_prop:
  forall (kx: k) (x: a) (l r:Map),
    Is_true (validsize (bin kx x l r)) ->
    Is_true (validsize_rec l) ->
    Is_true (validsize_rec r) ->
    Is_true (validsize_rec (bin kx x l r)).
intros kx x l r.
rewrite validsize_rec_expand.
generalize (validsize_rec l) (validsize_rec r) (validsize (bin kx x l r)).
intros b c d.
case b; case c; case d; auto.
  Qed.

Lemma realsize_bin:
  forall (k0: k) (a0: a) (l r: Map),
   realsize (bin k0 a0 l r) = 1 + realsize l + realsize r.
  auto.
  Qed.

  Lemma size_bin:
    forall (kx: k) (x: a) (l r: Map),
      size (bin kx x l r) = 1 + size l + size r.
    auto.
    Qed.

    Hint Rewrite realsize_bin size_bin: sbin.

Lemma validsize_realsize_size:
  forall (l: Map),
    Is_true (validsize l) ->
    realsize l = size l.
  compute [validsize].
  intro l.
  apply Nequal_equal.
  Qed.

  Hint Resolve
    validsize_rec_bin
    validsize_rec_self validsize_rec_hereditary1 validsize_rec_hereditary2.

Lemma validsize_singleR:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize_rec l) ->
    Is_true (validsize_rec r) ->
    Is_true (validsize_rec (singleR kx x l r)).
  intros kx x l r.
  intros one two.
  compute [singleR].
  destruct l.
  eauto.
  eauto.
Qed.

Lemma validsize_singleL:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize_rec l) ->
    Is_true (validsize_rec r) ->
    Is_true (validsize_rec (singleL kx x l r)).
  intros kx x l r.
  intros one two.
  compute [singleL].
  destruct r.
  eauto.
  eauto.
Qed.

Lemma validsize_doubleR:
  forall (t t4: Map)
    (k1: k) (x1: a),
    Is_true (validsize_rec t) ->
    Is_true (validsize_rec t4) ->
    Is_true (validsize_rec (doubleR k1 x1 t t4)).
  intros  t t4 k1 x1.
  intros tval val4.
  compute [doubleR].
  destruct t.
  eauto.
  destruct t2.
  eauto.
  apply validsize_rec_bin.
  eauto.
  eauto.
  Qed.

Lemma validsize_doubleL:
  forall (t t4: Map)
    (k1: k) (x1: a),
    Is_true (validsize_rec t) ->
    Is_true (validsize_rec t4) ->
    Is_true (validsize_rec (doubleL k1 x1 t t4)).
  intros  t t4 k1 x1.
  intros tval val4.
  compute [doubleL].
  destruct t4.
  eauto.
  destruct t4_1.
  eauto.
  apply validsize_rec_bin.
  eauto.
  eauto.
  Qed.

  
Lemma validsize_balanceLT:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize_rec l) ->
    Is_true (validsize_rec r) ->
    Is_true (validsize_rec (balanceLT kx x l r)).
  intros kx x l r.
  intros vl vr.
  compute [balanceLT].
  case (isBalanced r l).
  auto.
  compute [rotateR].
  destruct l.
  auto.
  case (nat_compare (lmap l1) (lmap l2)).
  apply validsize_singleR.
  auto.
  auto.
  apply validsize_doubleR.
  auto.
  auto.
  apply validsize_singleR.
  auto.
  auto.
  Qed.

Lemma validsize_balanceGT:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize_rec l) ->
    Is_true (validsize_rec r) ->
    Is_true (validsize_rec (balanceGT kx x l r)).
  intros kx x l r.
  intros vl vr.
  compute [balanceGT].
  case (isBalanced l r).
  auto.
  compute [rotateL].
  destruct r.
  auto.
  case (nat_compare (lmap r2) (lmap r1)).
  apply validsize_singleL.
  auto.
  auto.
  apply validsize_doubleL.
  auto.
  auto.
  apply validsize_singleL.
  auto.
  auto.
  Qed.

Lemma validsize_insert:
  forall (t: Map) (kx: k) (x: a),
    Is_true (validsize_rec t) ->
    Is_true (validsize_rec (insert kx x t)).
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
apply validsize_balanceLT.
apply IHt1.
eauto.
eauto.
intro irr.
clear irr.
eauto.
intro irr.
clear irr.
apply validsize_balanceGT.
eauto.
eauto.
Qed.


End map.



Extraction Language Haskell.
Recursive Extraction insert.