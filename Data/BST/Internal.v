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

Lemma validsize_singleR:
  forall (kx: k) (x: a) (l r: Map),
    Is_true (validsize_rec l) ->
    Is_true (validsize_rec r) ->
    Is_true (validsize_rec (singleR kx x l r)).
  intros kx x l.
  generalize kx x.
  clear kx x.
  destruct l.
  intros kx x r.
  intros lvalid rvalid.
  compute [singleR].
  compute [assert_false].
  apply validsize_rec_bin.
  exact lvalid.
  exact rvalid.
  intros kx x r.
  compute [singleR].
  compute [validsize].
  intros one two.
  assert (Is_true (validsize (bin k0 a0 l1 (bin kx x l2 r)))).
  apply equal_Nequal.
  assert (Is_true (validsize (Bin s k0 a0 l1 l2))).
  generalize one.
  clear one.
  compute [validsize_rec].
  case (validsize (Bin s k0 a0 l1 l2)).
  intro hoge.
  simpl.
  trivial.
  simpl.
  trivial.
  rename one into three.
  rename H into one.
  apply Nequal_equal in one.
  rename two into four.
  assert (Is_true (validsize r)) as two.
  apply validsize_rec_self.
  assumption.
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
  simpl.
  case (realsize l1).
  case (realsize l2).
  ring.
  intro p.
  case p.
  clear p.
  intro p.
  assert ((Npos (Psucc p)~0 + realsize r) = 1 + Npos p~1 + realsize r).
  assert (Npos (Psucc p)~0 = 1 + Npos p~1).
  simpl.
  reflexivity.
  rewrite H.
  reflexivity.
  rewrite H.
  ring.
  clear p.
  intro p.
  clear one x kx.
  assert ((Npos p~1 + realsize r) = 1 + Npos p~0 + realsize r).
  assert (Npos p~1 = 1 + Npos p~0).
  simpl.
  reflexivity.
  rewrite H.
  reflexivity.
  rewrite H.
  ring.
  ring.
  intro p.
  case p.
  case (realsize l2).
  clear p.
  intro p.
  ring.
  clear p.
  intros p q.
  case p.
  clear p.
  intro p.
  assert (Npos (Psucc p)~0 = 1 + Npos p~1).
  reflexivity.
  rewrite H.
  ring.
  clear p.
  intro p.
  clear kx.
  clear x.
  clear one.
  assert ((Npos p~1 + realsize r) =
    1 + Npos p~0 + realsize r).
  assert (Npos p~1 = 1 + Npos p~0).
  reflexivity.
  rewrite H.
  reflexivity.
  rewrite H.
  ring.
  ring.
  clear p.
  intro p.
  case (realsize l2).
  ring.
  Focus.
  intro q.
  case q.
  clear q.
  intro q.
  assert (
     (Npos (Psucc q)~0 + realsize r) =
     1 + Npos q~1 + realsize r).
  assert (
    Npos (Psucc q)~0 = 1 + Npos q~1
    ).
  reflexivity.
  rewrite H.
  reflexivity.
  rewrite H.
  clear H.
  ring.
  clear q.
  intro q.
  assert 
    ((Npos q~1) = 1 + Npos q~0).
  reflexivity.
  rewrite H.
  clear H.
  ring.
  ring.
  Unfocus.
  case (realsize l2).
  ring.
  clear p.
  intro p.
  case p.
  clear p.
  intro p.
  Focus.
  assert ((Npos (Psucc p)~0 + realsize r) = 1 + Npos p~1 + realsize r).
  assert (Npos (Psucc p)~0 = 1 + Npos p~1).
  reflexivity.
  rewrite H.
  ring.
  rewrite H.
  ring.
  Unfocus.
  clear p.
  intro p.
  Focus.
  assert (Npos p~1 = 1 + Npos p~0).
  reflexivity.
  rewrite H.
  ring.
  Unfocus.
  assert (
   (2 + realsize r) = 1 + 1 + realsize r
   ).
  reflexivity.
  rewrite H.
  ring.
  rewrite H.
  assert (
    realsize l1 + 1 + realsize l2  =
    size l1 + (1 + size l2)
    ).
  assert (
    realsize l1 + 1 + realsize l2 =
    realsize (Bin s k0 a0 l1 l2)
    ).
  simpl.
  case (realsize l1).
  ring.
  intro p.
  case p.
  clear p.
  intro p.
  reflexivity.
  clear p.
  intro p.
  reflexivity.
  reflexivity.
  rewrite H0.
  clear H0.
  clear one H.
  assert (
       realsize (Bin s k0 a0 l1 l2) = 1 + realsize l1 + realsize l2
       ).
  simpl.
  reflexivity.
  rewrite H.
  clear H.
  assert (realsize l1 = size l1).
  assert (Is_true (validsize l1)).
  apply validsize_rec_self.
  apply validsize_rec_hereditary1 with s k0 a0 l2.
  exact three.
  clear three.
  generalize H.
  clear H.
  compute [validsize].
  apply Nequal_equal.
  rewrite H.
  clear H.
  assert (realsize l2 = size l2).
  assert (Is_true (validsize l2)).
  apply validsize_rec_self.
  apply validsize_rec_hereditary2 with s k0 a0 l1.
  exact three.
  clear three.
  apply Nequal_equal.
  apply H.
  rewrite H.
  clear three H.
  ring.
  assert
    (realsize l1 + 1 + realsize l2 + realsize r =
      size l1 + (1 + size l2 + realsize r)).
  rewrite H0.
  clear H0.
  assert (
   size l1 + (1 + size l2 + realsize r) =
       size l1 + (1 + size l2) + realsize r).
  apply Nplus_assoc.
  rewrite H0.
  reflexivity.
  clear H0 H.
  clear three one.
  clear x k0 a0.
  assert (
       1 + realsize l1 + 1 + realsize l2 + realsize r =
       realsize l1 + 1 + realsize l2 + realsize r + 1).
  ring.
  rewrite H.
  clear H.
  assert (
    1 + size l1 + (1 + size l2 + realsize r)
    = 
    size l1 + (1 + size l2 + realsize r) + 1).
  ring.
  rewrite H.
  rewrite H1.
  reflexivity.
  assert (Is_true (validsize_rec l1)).
eapply validsize_rec_hereditary1 .
apply one.
assert (Is_true (validsize_rec (bin kx x l2 r))).
assert (Is_true (validsize_rec l2)).
eapply validsize_rec_hereditary2.
apply one.
assert (Is_true (validsize (bin kx x l2 r))).
compute [validsize].
apply equal_Nequal.
assert (realsize (bin kx x l2 r) = 1 + realsize l2 + realsize r) as H2.
auto.
rewrite H2.
clear H2.
assert (size (bin kx x l2 r) = 1 + size l2 + size r) as sequal.
auto.
rewrite sequal.
clear sequal.
assert (realsize l2 = size l2).
apply Nequal_equal.
apply validsize_rec_self.
assumption.
rewrite H2.
assert (realsize r = size r).
apply Nequal_equal.
apply validsize_rec_self.
assumption.
rewrite H3.
reflexivity.
rewrite validsize_rec_expand.
generalize two H1 H2.
generalize (validsize (bin kx x l2 r)) (validsize_rec l2)
  (validsize_rec r).
intros b c d.
case b; case c; case d; auto.
rewrite validsize_rec_expand.
generalize H H0 H1.
generalize (validsize_rec l1).
generalize (validsize_rec (bin kx x l2 r)).
generalize (validsize (bin k0 a0 l1 (bin kx x l2 r))).
intros b c d.
case b; case c; case d; auto.
Qed.



Lemma validsize_doubleR:
  forall (t t4: Map)
    (k1: k) (x1: a),
    Is_true (validsize_rec t) ->
    Is_true (validsize_rec t4) ->
    Is_true (validsize_rec (doubleR k1 x1 t t4)).
  
Definition doubleR : k -> a -> Map -> Map -> Map :=
  fun k1 x1 t t4 =>
    match t with
      | (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) =>
        bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
      | _ => assert_false k1 x1 t t4
    end.

  
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
  auto. 
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