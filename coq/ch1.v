From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.

(* functions *)
Definition f := fun n => n + 1.
About f.
Print f.
Locate "_ ^~ _".
Check f 3.
Fail Check f (fun x : nat => x).
Eval compute in f 3.

Definition repeat_twice (g : nat -> nat) : nat -> nat :=
  fun x => g (g x).
Eval compute in repeat_twice f 2.
Check (repeat_twice f).

(* data *)
Check true.
Definition f23 b := if b then 2 else 3.
Eval compute in f23 true.
Eval compute in f23 false.
Definition andb b1 b2 := if b1 then b2 else false.

Check fun x => f x.+1.
Locate ".+4".
Definition pred n := if n is u.+1 then u else n.
Definition pred5 n :=
  if n is u.+1.+1.+1.+1.+1 then u else 0.
Definition three_patterns n :=
  match n with
    u.+1.+1.+1.+1.+1 => u
  | v.+1 => v
  | 0 => n
  end.
Fail Definition wrong (n : nat) :=
  match n with 0 => true end.
Definition same_bool b1 b2 :=
  match b1, b2 with
  | true, true => true
  | _, _ => false
  end.
Definition same_bool2 b1 b2 :=
  match b1 with
  | true => match b2 with true => true | _ => false end
  | _ => false
  end.

(* recursion *)
Fixpoint addn n m :=
  if n is p.+1 then (addn p m).+1 else m.
Fixpoint addn2 n m :=
  match n with
  | 0 => m
  | p.+1 => (addn2 p m).+1
  end.
Fail Fixpoint loop n :=
  if n is 0 then loop n else 0.
Fixpoint subn m n : nat :=
  match m, n with
  | p.+1, q.+1 => subn p q
  | _ , _ => m
  end.
Fixpoint eqn m n :=
  match m, n with
  | 0, 0 => true
  | p.+1, q.+1 => eqn p q
  | _, _ => false
  end.
Notation "x == y" := (eqn x y).

(* containers *)
About cons.
Check cons 2 nil.
Check 1 :: 2 :: 3 :: nil.
Check fun l => 1 :: 2 :: 3 :: l.
Definition first_element_or_0 (s : seq nat) :=
  if s is a :: _ then a else 0.

Fixpoint size A (s : seq A) :=
  if s is _ :: tl then (size tl).+1 else 0.
Fixpoint map A B (f : A -> B) s :=
  if s is e :: tl then f e :: map f tl else nil.
Eval compute in [seq i.+1 | i <- [:: 2; 3]].
Check (3, false).
Eval compute in (true, false).1.

(* section *)
Section iterators.
 Variables (T : Type) (A : Type).
 Variables (f : T -> A -> A).
 Implicit Type x : T.
 Fixpoint iter n op x :=
   if n is p.+1 then op (iter p op x) else x.
 Fixpoint foldr a s :=
   if s is x :: xs then f x (foldr a xs) else a.
 About foldr.
 Variable init : A.
 Variables x1 x2 x3 : T.
  Eval compute in foldr init [:: x1; x2; x3].
End iterators.
About iter.
About foldr.
Eval compute in iter 5 pred 7.
Eval compute in foldr addn 0 [:: 1; 2; 3].
Fixpoint add m n := if m is u.+1 then add u n.+1 else n.
Section symbolic.
  Variable n : nat.
  Eval simpl in pred (add n.+1 7).
  Eval simpl in pred (addn n.+1 7).
End symbolic.

(* iterated ops *)
Fixpoint iota m n := if n is u.+1 then m :: iota m.+1 u else [::].
Notation "\sum_ ( m <= i < n ) F" :=
  (foldr (fun i a => F + a) 0 (iota m (n-m))).
Eval compute in \sum_( 1 <= i < 5 ) (i * 2 - 1).
Eval compute in \sum_( 1 <= i < 5 ) i.

(* aggregated data *)
Record point := Point { x : nat; y : nat; z : nat }.
Eval compute in x (Point 3 0 2).
Eval compute in y (Point 3 0 2).

(** Exercises *************************************** *)

(* pair *)

Inductive pair (A B : Type) := mk_pair (a : A) (b : B).

Notation "( A , B )" := (mk_pair A B).

Definition fst A B (p : pair A B) := let: (x, _) := p in x.

Eval compute in fst (4,5).

(* iter add *)
Definition addn n1 n2 := iter n1 S n2.

Eval compute in addn 3 4.

(* iter mul *)
Definition muln n1 n2 := iter n1 (addn n2) 0.

Eval compute in muln 3 4.

(* nth *)
Fixpoint nth T (def : T) (s : seq T) n :=
  if s is x :: s' then if n is n'.+1 then nth def s' n' else x else def.

Eval compute in nth 0 [:: 3; 7; 11; 22] 2.
Eval compute in nth 0 [:: 3; 7; 11; 22] 7.

(* rev *)
Fixpoint catrev T (s1 s2 : seq T) :=
  if s1 is x :: xs then catrev xs (x :: s2) else s2.

Definition rev T (s : seq T) := catrev s [::].

Eval compute in rev [:: 1; 2; 3].

(* flatten *)
Definition flatten T (s : seq (seq T)) := foldr cat [::] s.

Eval compute in
  flatten [:: [:: 1; 2; 3]; [:: 4; 5] ].

(* all_words *)
Definition all_words n T (alphabet : seq T) :=
  let prepend x wl := [seq x :: w | w <- wl] in
  let extend wl := flatten [seq prepend x wl | x <- alphabet] in
  iter n extend [::[::]].

Eval compute in all_words 2 [:: 1; 2; 3].

