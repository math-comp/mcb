From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.

(* 1.1 Functions *)
(* 1.1.1 Defining functions, performing computation *)

Definition f1 := fun n => n + 1.
Definition f2 n := n + 1.
Definition f (n : nat) : nat := n + 1.

About f.
Print f.
Locate "_ ^~ _".
Check f 3.
Fail Check f (fun x : nat => x).
Eval compute in f 3.


(* 1.1.2 Functions with several arguments *)

Definition g' (n : nat) (m : nat) : nat := n + m * 2.
Definition g (n m : nat ) : nat := n + m * 2.

About g.

Definition h (n : nat) : nat -> nat := fun m => n + m * 2.

About h.

Print g.
Print h.

Check g 3.
Compute g 2 3.


(* 1.1.3 Higher-order functions *)

Definition repeat_twice (g : nat -> nat) : nat -> nat :=
  fun x => g (g x).
Eval compute in repeat_twice f 2.
Check (repeat_twice f).
Fail Check (repeat_twice f f).


(* 1.1.4 Local definitions *)

Compute
  let n := 33 : nat in
  let e := n + n + n in
    e + e + e.


(* 1.2 Data types, first examples *)
(* 1.2.1 Boolean values *)

(* This definition is already imported *)
(* Inductive bool := true | false. *)

Check true.

Definition twoVthree (b : bool) := if b then 2 else 3.

Eval compute in twoVthree true.
Eval compute in twoVthree false.

Definition andb (b1 b2 : bool) := if b1 then b2   else false.
Definition orb  (b1 b2 : bool) := if b1 then true else b2.


(* 1.2.2 Natural Numbers *)
(* Inductive nat := O | S (n : nat). *)

Check fun x => f x.+1.

Definition non_zero n := if n is p.+1 then true else false.

Compute non_zero 5.

Locate ".+4".

Definition pred n := if n is u.+1 then u else O.

Definition larger_then_4 n :=
  if n is u.+1.+1.+1.+1.+1 then true else false.

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
  | false, false => true
  | _, _ => false
  end.

Definition same_bool2 b1 b2 :=
  match b1 with
  | true => match b2 with true => true | _ => false end
  | false => match b2 with true => false | _ => true end
  end.


(* 1.2.3 Recursion on natural numbers *)

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

Compute O == O.
Compute 1 == S O.
Compute 1 == O.+1.
Compute 2 == S O.
Compute 2 == 1.+1.
Compute 2 == addn 1 O.+1.

Definition leq m n := m - n == 0.
Notation "m <= n" := (leq m n).


(* 1.3 Containers *)

Inductive listn : Set := niln | consn (hd : nat) (tl : listn).

Check consn 1 (consn 2 niln).

Inductive testbool : Set := ttrue | tfalse.

Fail Check consn ttrue (consn tfalse niln).

Inductive listb := nilb | consb (hd : testbool) (tl : listb).


(* 1.3.1 The polymorphic sequence data type *)

(* This definition is already imported. *)
(* Inductive seq (A : Type) := nil | cons (hd : A) (tl : seq A). *)

About cons.
Check cons 2 nil.
Check 1 :: 2 :: 3 :: nil.
Check fun l => 1 :: 2 :: 3 :: l.

Definition head T (x0 : T) (s : seq T) := if s is x :: _ then x else x0.


(* 1.3.2 Recursion for sequences *)

Fixpoint size A (s : seq A) :=
  if s is _ :: tl then (size tl).+1 else 0.

Fixpoint map A B (f : A -> B) s :=
  if s is e :: tl then f e :: map f tl else nil.

Notation "[ 'seq' E | i <- s ]" := (map (fun i => E) s).

Eval compute in [seq i.+1 | i <- [:: 2; 3]].

(* This definition is already imported *)
(* Inductive option A := None | Some (a : A). *)

Definition only_odd (n : nat) : option nat :=
  if odd n then Some n else None.

Definition ohead (A : Type) (s : seq A) :=
  if s is x :: _ then Some x else None.

Inductive pair (A B : Type) : Type := mk_pair (a : A) (b : B).

(* These definitions are already imported *)
(* Notation "( a , b )" := (mk_pair a b). *)
(* Notation "A * B" := (pair A B). *)
(* Definition fst A B (p : pair A B) := *)
(*   match p with mk_pair x _ => x end. *)

Check (3, false).
Compute (true, false).1.


(* 1.1.4 Aggregating data in record types *)

Record point : Type := Point { x : nat; y : nat; z : nat }.

Compute x (Point 3 0 2).
Compute y (Point 3 0 2).


(* 1.4 The Section mechanism *)

Section iterators.
  
  Variables (T : Type) (A : Type).
  Variables (f : T -> A -> A).
  
  Implicit Type x : T.
  
  Fixpoint iter n op x :=
    if n is p.+1 then op (iter p op x) else x.
  
  Fixpoint foldr a s :=
    if s is x :: xs then f x (foldr a xs) else a.
  
  About foldr.
  
End iterators.

About foldr.

Eval compute in iter 5 predn 7.
Eval compute in foldr addn 0 [:: 1; 2 ].


(* 1.5 Symbolic computation *)

Section iterators_symbolic.

  Variables (T : Type) (A : Type).
  Variables (f : T -> A -> A).

  Fixpoint foldr' a s :=
    if s is x :: xs then f x (foldr' a xs) else a.

  About foldr'.

  Variable init : A.
  Variables x1 x2 : T.
  Compute foldr' init [:: x1; x2].

  Compute addn 1 (addn 2 0).

  (* already defined *)
  (* Fixpoint addn n m := if n is p.+1 then (addn p m).+1 else m. *)

  Fixpoint add n m := if n is p.+1 then add p m.+1 else m.

  Variable n : nat.
  Eval simpl in predn (add n.+1 7).
  Eval simpl in predn (addn n.+1 7).

End iterators_symbolic.


(* 1.6 Iterators and mathematical notations *)

Fixpoint iota m n := if n is u.+1 then m :: iota m.+1 u else [::].
Notation "\sum_ ( m <= i < n ) F" :=
  (foldr (fun i a => F + a) 0 (iota m (n-m))).

Eval compute in \sum_( 1 <= i < 5 ) (i * 2 - 1).
Eval compute in \sum_( 1 <= i < 5 ) i.


(* 1.7 Notations, abbreviations *)
Notation "m + n" := (addn m n) (at level 50, left associativity).
Notation "m <= n" := (leq m n) (at level 70, no associativity).
Notation "m < n" := (m.+1 <= n).
Notation "n > m" := (m.+1 <= n) (only parsing).
Notation succn := S.
Notation "n .+1" := (succn n) (at level 2, left associativity): nat_scope.

Locate "<=".

(* all_words, we will face later in the book *)

Definition all_words n T (alphabet : seq T) :=
  let prepend x wl := [seq x :: w | w <- wl] in
  let extend wl := flatten [seq prepend x wl | x <- alphabet] in
  iter n extend [::[::]].

Eval compute in all_words 2 [:: 1; 2; 3].


(******** Code below is not in the book already ********)

(** Exercises *************************************** *)

Module exercises.
  (* pair *)

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
End exercises.
