From mathcomp Require Import all_ssreflect.


Check 3 : nat.

Section CheckNat.

Variable n : nat.
Check n + n : nat.

Fail Check n + n : bool.

Check muln0 n : n * 0 = 0.

End CheckNat.

Check Prop.

Lemma stamps n : 12 <= n -> exists s4 s5, s4 * 4 + s5 * 5 = n.
Proof.
have [m leq_mn] := ubnPgeq n; elim: n => // n IHn in m leq_mn *; first by case: n => [|n] in IHn *.
do 12! [ case: m => //= m in leq_mn * ] => _.
case: m => [|m] in leq_mn *; first by exists 3, 0.
case: m => [|m] in leq_mn *; first by exists 2, 1.
case: m => [|m] in leq_mn *; first by exists 1, 2.
case: m => [|m] in leq_mn *; first by exists 0, 3.
case: (IHn ((16+m) - 4) _ isT) => [|s4 [s5 def_m4]]. 
  by rewrite leq_subLR (leq_trans leq_mn) // addSnnS leq_addl.
by exists s4.+1, s5; rewrite mulSn -addnA def_m4 subnKC.
Qed.


Fixpoint nat_ind (P : nat -> Prop)
  (p0 : P 0) (pS : forall n : nat, P n -> P n.+1) n : P n :=
  if n is m.+1 then
    let pm (* : P m *) := nat_ind P p0 pS m in
    pS m pm (* : P m.+1 *)
  else p0.

Lemma absurd : false = true -> forall P, P.
Proof. by []. Qed. (* see coqart? *)

Definition leq_trans n m o (Hmn : m <= n) (Hno : n <= o) : m <= o :=
  nat_ind (fun n => forall m o, m <= n -> n <= o -> m <= o)
    (fun m => match m with
      | 0 => fun o Hmn Hno => (isT : 0 <= o)
      | pm.+1 => fun o (Hmn : pm.+1 <= 0) Hno => absurd Hmn (pm.+1 <= o)
      end)
    (fun pn (IHn : forall m o : nat, m <= pn -> pn <= o -> m <= o) =>
      fun m => match m with
       | 0 => fun o Hmn Hno => (isT : 0 <= o)
       | pm.+1 => fun o (Hmn : pm.+1 <= pn.+1) => match o with
                   | 0 => fun (Hno : pn.+1 <= 0) => absurd Hno (pm.+1 <= 0)
                   | po.+1 => fun (Hno : pn.+1 <= po.+1) =>
                       IHn pm po (Hmn : pm <= pn) (Hno : pn <= po)
                   end
       end)
    n
  m o Hmn Hno.

Definition strong_nat_ind (P : nat -> Prop)
  (base : P 0)
  (step : forall n, (forall m, m <= n -> P m) -> P n.+1) n : P n
:=
  nat_ind (fun n => forall m, m <= n -> P m)
    (fun m => match m with
      | 0 => fun Hmn => base
      | pm.+1 => fun (Hmn : pm.+1 <= 0) => absurd Hmn (P pm.+1)
      end)
    (fun pn (IHn : forall m, m <= pn -> P m) =>
      fun m => match m with
       | 0 => fun Hmn => base
       | pm.+1 => fun (Hmn : pm.+1 <= pn.+1) =>
            let P_upto_pm j (Hjm : j <= pm) : P j :=
              IHn j (leq_trans pm j pn Hjm (Hmn : pm <= pn)) in
            step pm P_upto_pm
       end)
    n
  n (leqnn n : n <= n).

Axiom P : nat -> Prop.
Check (nat_ind (fun n => forall m, m <= n -> P m)).
(*
: (forall m : nat, m <= 0 -> P m) ->
(forall n : nat,
 (forall m : nat, m <= n -> P m) -> forall m : nat, m <= n.+1 -> P m) ->
forall n m : nat, m <= n -> P m
*)
Lemma  strong_nat_ind2 (P : nat -> Prop)
  (base : P 0)
  (step : forall n, (forall m, m <= n -> P m) -> P n.+1) x : P x.
Proof.
apply: (nat_ind (fun n => forall m, m <= n -> P m) _ _ x x (leqnn x)).
  Show.
  by case=> [_| //]; exact: base.
move=> n IHn; case=> [_|m Hm]; first by exact: base.
apply: step=> j Hjm; apply: IHn.
apply: leq_trans Hjm Hm.
Qed.