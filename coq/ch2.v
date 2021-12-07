From mathcomp Require Import all_ssreflect.


(* 2.1.1 Ground equalities *)

Check 3 = 3.
Check false || true = true.

Locate "=".
About eq.

Fail Check 3 = [:: 3].

Lemma my_first_lemma : 3 = 3.
Proof. Admitted.

About my_first_lemma.


(* 2.1.2 Identities *)

(* Lemma addnA (m n k : nat) : m + (n + k) = m + n + k. Admitted. *)
(* Lemma addnA m n k : m + (n + k) = m + n + k. Admitted. *)
(* Lemma orbT b : b || true = true. Admitted. *)

(* Lemma orbA b1 b2 b3 : b1 || (b2 || b3) = b1 || b2 || b3. Admitted. *)
(* Lemma implybE a b : (a ==> b) = ~~ a || b. Admitted. *)
(* Lemma negb_and (a b : bool) : ~~ (a && b) = ~~ a || ~~ b. Admitted. *)


(* 2.1.3 From boolean predicates to formal statements *)

(* Lemma leq0n (n : nat) : 0 <= n = true. Admitted. *)
(* Lemma leq0n (n : nat) : 0 <= n. Admitted. *)

(* Lemma eqn_leq m n : (m == n) = (m <= n) && (n <= m). Admitted. *)
(* Lemma neq_ltn m n : (m != n) = (m < n) || (n < m). Admitted. *)
(* Lemma leqn0' n : (n <= 0) = (n == 0). Admitted. *)
(* Lemma dvdn1 d : (d %| 1) = (d == 1). Admitted. *)
(* Lemma odd_mul m n : odd (m * n) = odd m && odd n. Admitted. *)

(* Lemma leq_pmull m n : n > 0 -> m <= n * m. Admitted. *)
(* Lemma odd_gt0 n : odd n -> n > 0. Admitted. *)

(* Lemma dvdn_mul d1 d2 m1 m2 : d1 %| m1 -> d2 %| m2 -> d1 * d2 %| m1 * m2. Admitted. *)


(* 2.2 Formal proofs *)

Lemma my_first_lemma' : 3 = 3.
Proof. Admitted.


(* 2.2.1 Proofs by computation *)

Lemma my_first_lemma'' : 3 = 3.
Proof. by []. Qed.

About my_first_lemma''.
    
Lemma my_second_lemma : 2 + 1 = 3.
Proof. by []. Qed.

Lemma addSn m n : m.+1 + n = (m + n).+1.
Proof. by []. Qed.


(* 2.2.2 Case analisys *)

Definition negb (b : bool) : bool := if b then false else true.

Lemma negbK b : ~~ (~~ b) = b.
Proof. by case: b. Qed.

Lemma leqn0 n : (n <= 0) = (n == 0).
Proof. by case: n => [| k]. Qed.

Fixpoint muln (m n : nat) : nat :=
  if m is p.+1 then n + muln p n else 0.


(* 2.2.3 Rewriting *)

Lemma muln_eq0 m n : (m * n == 0) = (m == 0) || (n == 0).
Proof.
  case: m => [|m] //.
  case: n => [|k] //.
  by rewrite muln0.
Qed.

Lemma leqE m n : (m <= n) = (m - n == 0).
Proof. by []. Qed.

Lemma leq_mul2l' m n1 n2 : (m * n1 <= m * n2) = (m == 0) || (n1 <= n2).
Proof. by rewrite !leqE -mulnBr muln_eq0. Qed.


(* 2.3 Quantifiers *)
(* 2.3.1 Univerdal quantification, first examples *)

About leq.

Definition leq := fun (n m : nat) => m - n == 0.

About leqn0.
About muln_eq0.

Lemma seq_eq_ext (s1 s2 : seq nat) :
  size s1 = size s2 ->
  (forall i : nat, nth 0 s1 i = nth 0 s2 i) ->
  s1 = s2. Admitted.

Lemma size_map (T1 T2 : Type) :
  forall (f : T1 -> T2) (s : seq T1), size (map f s) = size s. Admitted.

(* This definition is already imported *)
(* Definition commutative (S T : Type) (op : S -> S -> T) := *)
(*   forall x y, op x y = op y x. *)

Lemma addnC : commutative addn. Admitted.

Section StandardPredicates.
  Variable T : Type.
  
  Implicit Types (op add : T -> T -> T).
  
  Definition associative op := forall x y z, op x (op y z) = op (op x y) z.
  Definition left_distributive op add :=
    forall x y z, op (add x y) z = add (op x z) (op y z).
  Definition left_id e op := forall x, op e x = x.
End StandardPredicates.

Section MoreStandardPredicates.
  Variables rT aT : Type.
  Implicit Types (f : aT -> rT).
  Definition injective f := forall x1 x2, f x1 = f x2 -> x1 = x2.
  Definition cancel f g := forall x, g (f x) = x.
  Definition pcancel f g := forall x, g (f x) = Some x.
End MoreStandardPredicates.

About commutative.

Check 3 = 3.
Check (commutative addn).

(* 2.3.2 Organizing proofs with sections *)

Section Chinese.
  
  Variables m1 m2 : nat.
  Hypothesis co_m12 : coprime m1 m2.

  Lemma chinese_remainder x y :
    (x == y %[mod m1 * m2]) = (x == y %[mod m1]) && (x == y %[mod m2]).
  Proof. Admitted.
  
End Chinese.

About chinese_remainder.


(* 2.3.3 Using lemmas in proofs *)

Lemma leqnn n : n <= n. Proof. Admitted.

Lemma example a b : a + b <= a + b.
Proof. by apply: leqnn. Qed.

Lemma example1 a b : a.+1 + b <= (a + b).+1.
Proof. by apply: leqnn. Qed.

(* This line belongs to the file where leqnn is stated and proved.*)
Hint Resolve leqnn.
Lemma example2 a b : a + b <= a + b.
Proof. by []. Qed.

(* Lemma contraLR (c b : bool) : (~~ c -> ~~ b) -> (b -> c). *)
(* Lemma dvdn_addr m d n : d %| m -> (d %| m + n) = (d %| n). *)
(* Lemma dvdn_fact m n : 0 < m <= n -> m %| n ` !. *)
(* Lemma prime_gt0 p : prime p -> 0 < p. *)
(* Lemma gtnNdvd n d : 0 < n -> n < d -> (d %| n) = false. *)
(* Lemma prime_gt1 p : prime p -> 1 < p. *)
  
Lemma example3 m p : prime p -> p %| m `! + 1 -> m < p.
Proof.
  move=> prime_p.
  apply: contraLR.
  rewrite -leqNgt.
  move=> leq_p_m.
  rewrite dvdn_addr.
    rewrite gtnNdvd.
      by []. (* ~~ false *)
      by []. (* 0 < 1 *)
    by apply: prime_gt1. (* 1 < p *)
  apply: dvdn_fact.
  rewrite prime_gt0. (* 0 < p <= n *)
    by []. (* true && p <= m *)
  by []. (* prime p *)
Qed.

Lemma example3' m p : prime p -> p %| m `! + 1 -> m < p.
Proof.
  move=> prime_p; apply: contraLR; rewrite -leqNgt; move=> leq_p_m.
  rewrite dvdn_addr.
    by rewrite gtnNdvd // prime_gt1.
  by rewrite dvdn_fact // prime_gt0.
Qed.


(* 2.3.4 Proofs by induction *)

About nat_ind.

About list_ind.

About last_ind.

About foldl.
Print foldl.

(* Lemma last_ind A (P : list A -> Prop) : *)
(* P [::] -> (forall s x, P s -> P (rcons s x)) -> forall s, P s. *)

(* Fixpoint foldl T R (f : R -> T -> R) z s := *)
(*   if s is x :: s' then foldl f (f z x) s' else z. *)

(* Lemma cats1 T s (z : T) : s ++ [:: z] = rcons s z. *)

(* Lemma foldr_cat T R f (z0 : R) (s1 s2 : seq T) : *)
(*   foldr f z0 (s1 ++ s2) = foldr f (foldr f z0 s2) s1. *)

(* Lemma rev_rcons T s (x : T) : rev (rcons s x) = x :: rev s. *)

Lemma foldl_rev T A f (z : A) (s : seq T) :
foldl f z (rev s) = foldr (fun x z => f z x) z s .
Proof.
  elim/last_ind: s z => [|s x IHs] z //.
  by rewrite -cats1 foldr_cat -IHs cats1 rev_rcons.
Qed.


(* 2.4 Rewrite, a Swiss army knife *)

Lemma example3'' m p : prime p -> p %| m `! + 1 -> m < p.
Proof.
  move=> prime_p; apply: contraLR; rewrite -leqNgt => leq_p_m.
  rewrite dvdn_addr ?dvdn_fact ?prime_gt0 //.
    by rewrite gtnNdvd // prime_gt1.
Qed.

Lemma silly_example n : n + 0 = (n + 0) + 0.
Proof. by rewrite [in RHS]addn0. Qed.

Lemma simplify_me : size [:: true] = 1.
Proof. by rewrite /=. Qed.


(* 2.4.1 Rewrite contextual patterns *)

Lemma leq_mul2l m n1 n2 : (m * n1 <= m * n2) = (m == 0) || (n1 <= n2).
Proof.
  (* rewrite [n1 <= _]leqE. *)
  (* rewrite -mulnBr. *)
  rewrite [in RHS]leqE.
  rewrite -[_ || _]muln_eq0.
  rewrite -[n1]/(0 + n1).
  rewrite [in LHS]/ssrnat.leq.
Admitted.


(* 2.5 Searching the library *)

Search (odd _).
Search eq odd -coprime.

(* We will need all_words and size_all_words at the end of the book *)
  
Definition all_words n T (alphabet : seq T) :=
  let prepend x wl := [seq x :: w | w <- wl] in
  let extend wl := flatten [seq prepend x wl | x <- alphabet] in
  iter n extend [::[::]].
Arguments all_words _ [T] _.


Lemma size_all_words n T (alphabet : seq T) :
  size (all_words n alphabet) = size alphabet ^ n.
Proof.
  elim: n => [|n IHn]; first by rewrite expn0.
  rewrite expnS -{}IHn [in LHS]/all_words iterS -/(all_words _ _).
  elim: alphabet (all_words _ _) => //= w ws IHws aw.
    by rewrite size_cat IHws size_map mulSn.
Qed.


(******** Code below is not in the book already ********)

(* Code below is not anymore in the book and given here for *)
(* additional examples.                                     *)

(** Exercises *)

Lemma orbA b1 b2 b3 : b1 || (b2 || b3) = b1 || b2 || b3.
Proof. by case: b1; case: b2; case: b3. Qed.

Lemma implybE a b : (a ==> b) = ~~ a || b.
Proof. by case: a; case: b. Qed.

Lemma negb_and (a b : bool) : ~~ (a && b) = ~~ a || ~~ b.
Proof. by case: a; case: b. Qed.

Lemma subn_sqr m n : m ^ 2 - n ^ 2 = (m - n) * (m + n).
Proof. by rewrite mulnBl !mulnDr addnC [m * _]mulnC subnDl !mulnn. Qed.

Lemma odd_exp m n : odd (m ^ n) = (n == 0) || odd m.
Proof.
  elim: n => // n IHn.
  rewrite expnS odd_mul {}IHn orbC.
    by case: odd.
Qed.
