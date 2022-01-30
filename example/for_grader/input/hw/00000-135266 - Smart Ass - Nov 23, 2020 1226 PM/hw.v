
(** Exercise: 2 points (plus_assoc) *)
Theorem plus_assoc : forall m n p : nat,
    m + (n + p) = (m + n) + p.
Proof.
  induction m; intros; simpl; eauto.
Qed.

Module Number.
  (** Exercise: 1 point (plus_n_O) *)
  Theorem plus_n_O : forall n : nat,
      n = n + 0.
  Proof.
    induction n; eauto.
  Qed.

End Number.

(** Bonus question: 5 bonus points (plus_very_hard) *)
Theorem plus_very_hard : 0 + 0 = 0.
Proof.
  reflexivity.
Qed.

(** We define a double function. *)
Fixpoint double (n:nat) :=
  match n with
  | O => O
  | S n' => S (S (double n'))
  end.

(** Also the even predicate. *)
Inductive ev : nat -> Prop :=
| ev_0 : ev 0
| ev_SS : forall n : nat, ev n -> ev (S (S n)).

Lemma plus_n_Sm : forall n m : nat,
  S (n + m) = n + (S m).
Proof.
  induction n; simpl; eauto.
Qed.

(** Exercise: 3 points (plus_plus_double) *)
Theorem plus_plus_double : forall n : nat,
    n + n = double n.
Proof.
  induction n; simpl; eauto.
  rewrite <- IHn.
  rewrite <- plus_n_Sm.
  reflexivity.
Qed.

(** Exercise: 3 points (double_ev) *)
Theorem double_ev : forall n : nat,
    ev (double n).
Proof.
  induction n; eauto using ev.
Qed.

(** Exercise: 1 points (test_plus) *)
Example test_plus1 : plus 1 1 = 2.
Proof.
  reflexivity. Qed.

Example test_plus2 : plus 1 2 = 3.
Proof.
  reflexivity. Qed.

Example test_plus3 : plus 1 3 = 4.
Proof.
  reflexivity. Qed.


(** Exercise: 1 points (test_double) *)
Example test_double1 : double 1 = 2.
Proof.
  reflexivity. Qed.

Example test_double2 : double 2 = 4.
Proof.
  reflexivity. Qed.

Example test_double3 : double 3 = 6.
Proof.
  reflexivity. Qed.

(* Exercise: 2 point (leb) *)
Fixpoint leb (n m : nat) : bool :=
  match n, m with
  | 0, _ => true
  | S n', 0 => false
  | S n', S m' => leb n' m'
  end.

(* Exercise: 2 point (leb_refl) *)
Theorem leb_refl : forall n, leb n n = true.
Proof.
  induction n; eauto.
Qed.

(* Exercise: 3 point (leb_succ) *)
(* State and prove that [n] is smaller than or equal to [S n]. *)
Theorem leb_succ :
  forall n, true = leb n (S n).
Proof.
  induction n; eauto.
Qed.

Lemma leb_trans : forall m n p,
    leb m n = true -> leb n p = true -> leb m p = true.
Proof.
  induction m; intros.
  - eauto.
  - destruct n, p; eauto; easy.
Qed.

(* Exercise: 3 point (leb_double) *)
(* State and prove that [n] is smaller than or equal to [double n]. *)
Theorem leb_double :
  forall n, leb n (double n) = true.
Proof.
  induction n.
  simpl. reflexivity.
  simpl.
  eapply leb_trans.
  eauto.
  symmetry.
  apply leb_succ.
Qed.

(* Exercise: 10 point (plus_1) *)
Theorem plus_1 : forall n, plus n 1 = S n.
Proof.
  induction n; simpl; eauto.
Qed.
