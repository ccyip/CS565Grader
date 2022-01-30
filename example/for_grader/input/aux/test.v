(** Import [graderlib] which also exports [local.v]. *)
Require Import graderlib.
Require Import Lia.

(** DO NOT use [Require Import]! *)
Require hw.

Goal True.
  (* Must start with this line. The homework name must be the same as the one we
  just imported. *)
  start "hw".

  (* Now we can specify some options. All these options are optional. *)
  (* This option means all axioms under the namespace [Maps] and [Tactics] are
  allowed. The namespaces are given by this comma-separated string. This option
  is useful when the students can use unproved exercises from software
  foundations. *)
  allowed_module_axioms "Maps,Tactics".
  (* This option specifies what axioms they can use. Comma-separated. *)
  allowed_axioms "functional_extensionality".
  (* Make sure these axioms and namespaces are consistent with what [Print
  Assumptions] prints out. If you have not imported the module, you may need
  the full name in the list of axioms. *)
  (* Another note is that all exercises are considered allowed axioms, meaning
  that an exercise can get full mark even if it uses a previous exercise that
  the student couldn't prove. *)
  (* This option specifies which exercises are bonus. DO NOT include the [hw]
  namespace. Comma-separated. *)
  bonus "plus_very_hard".


  (* These two lines are required. They print out the penalty and comment in
  [local.v]. *)
  print_local_option "penalty" Penalty.
  print_local_option "comment" Comment.
  nl.

  (* Before we process the exercises, we should make sure that the students do
  not cheat by changing the definitions provided in the homework. Call me
  paranoid, but why trust human when mechanization exists. *)
  (* If the definition is a function, we can use [unify] tactic to check. We can
  print out [hw.double] using shortcut or [Print hw.double], and then copy-paste
  the definition. *)
  unify hw.double (fix double (n : nat) : nat :=
                     match n with
                     | 0 => 0
                     | S n' => S (S (double n'))
                     end).
  (* If the definition is an inductive definition, we can simply check the type
  of its eliminator, e.g., the generated [_ind] function. Again, we can print
  out the type of [hw.ev_ind] using shortcut or [Check hw.ev_ind], and then
  copy-paste the output. *)
  check_type hw.ev_ind (forall P : nat -> Prop,
                           P 0 ->
                           (forall n : nat, hw.ev n -> P n -> P (S (S n))) ->
                           forall n : nat, hw.ev n -> P n).
  (* A failing check will fail the whole compilation. In this case, go fix their
  homework manually and apply penalty if it's malicious. *)

  (* Now we start processing the exercises. *)
  (* First, we define an exercise. The name MUST be the same as the
  theorem/definition name. The grader uses this name for a few things. For
  example, the latter exercises can still get full mark if they use this
  exercise without finishing it. *)
  exc "plus_assoc".
  (* Define the score. This exercise is worth 2 points. *)
  pt 2.
  (* Make sure the students don't change the type. It would be trivial if they
  simply change the type to [True]. *)
  check_type @hw.plus_assoc (forall m n p : nat, m + (n + p) = m + n + p).
  (* Now we define a _complete verifier_: they get full mark if pass the
  verifier, or zero mark otherwise. *)
  (* For a proof exercise, typically we just need to check the axioms it uses.
  The verifier succeeds if it prints out [Closed under global context] (or uses
  only allowed axioms). If the proof is admitted, the verifier also fails. *)
  ass.
  Print Assumptions hw.plus_assoc.
  nl.
  (* Now we just finished processing an exercise! *)

  (* If the exercise is inside a module, the exercise name should include the
  namespace! *)
  exc "Number.plus_n_O".
  pt 1.
  check_type @hw.Number.plus_n_O (forall n : nat, n = n + 0).
  ass.
  Print Assumptions hw.Number.plus_n_O.
  nl.

  (* We do not need to do anything special to bonus question, besides the bonus
  option above. *)
  exc "plus_very_hard".
  pt 5.
  check_type @hw.plus_very_hard (0 + 0 = 0).
  ass.
  Print Assumptions hw.plus_very_hard.
  nl.

  (* The following two exercises are similar to previous ones. Just remember to
  check the definitions of [double] and [ev] above! *)
  exc "plus_plus_double".
  pt 3.
  check_type @hw.plus_plus_double (forall n : nat, n + n = hw.double n).
  ass.
  Print Assumptions hw.plus_plus_double.
  nl.

  exc "double_ev".
  pt 3.
  check_type @hw.double_ev (forall n : nat, hw.ev (hw.double n)).
  ass.
  Print Assumptions hw.double_ev.
  nl.

  (* Some exercises may contain a few parts. We can declare them with
  "subexercises". *)
  (* We add a "#" before the name because this exercise does not correspond to
  any definitions/theorems. We do not need to do it, but I usually do it to
  avoid confusion. *)
  exc "#test_plus".
  pt 1.
  nl.
  (* The way we handle subexercises is the same as normal exercises, but we use
  the command [itm] and do not specify score. If all items pass the check, they
  get full mark, or they get zero (if I remember correctly). *)
  itm "test_plus1".
  check_type @hw.test_plus1 (1 + 1 = 2).
  ass.
  Print Assumptions hw.test_plus1.
  nl.

  itm "test_plus2".
  check_type @hw.test_plus2 (1 + 2 = 3).
  ass.
  Print Assumptions hw.test_plus2.
  nl.

  itm "test_plus3".
  check_type @hw.test_plus3 (1 + 3 = 4).
  ass.
  Print Assumptions hw.test_plus3.
  nl.

  (* However, if we do not need to refer back to these subexercises (e.g., no
  proof depends on these items), I tend to simply use a "flat" exercise, because
  at some point I found the subexercises system a bit finicky. *)
  exc "#test_double".
  pt 1.
  check_type @hw.test_double1 (hw.double 1 = 2).
  check_type @hw.test_double2 (hw.double 2 = 4).
  check_type @hw.test_double3 (hw.double 3 = 6).
  ass.
  Print Assumptions hw.test_double1.
  Print Assumptions hw.test_double2.
  Print Assumptions hw.test_double3.
  nl.

  (* Some exercises might be open questions: there are different correct
  answers. These exercises may require manual grading. *)
  exc "leb".
  pt 2.
  (* We print out the manually graded score in [local.v]. If we have not filled
  in the score yet, the grader will output a list of ungraded items, otherwise
  it will simply use the graded score. *)
  print_manual_grade mg_leb.
  check_type @hw.leb (nat -> nat -> bool).
  (* Technically we can end this exercise here with [nl]. But manually grading
  all students' exercises can be time consuming. We can "try our best" to grade
  their work automatically, using _sound verifiers_ or _sound falsifiers_. *)
  (* To declare a sound verifier, we use the command [verifier]. If the exercise
  passes the sound verifier, they get full mark. Otherwise we are not sure, so
  the grader will try the next verifier or falsifier if any. If no more verifier
  or falsifier is available, this exercise falls back to manual grading. *)
  (* But what does it mean by passing a verifier? We say a sound verifier
  succeeds if:

  1. no error is reported, and

  2. if we use [ass] and [Print Assumptions], it only prints out allowed axioms.

  You can use the [error] tactic to emit an error with string message, which
  will be reported to the students as comments. Some other tactics may
  internally use [error] to report errors too. See [gradelib.v]. We will talk
  more about falsifier later. *)
  verifier.
  (* One way we can implement the verifier is to show the student answer is
  equivalent to a reference answer. *)
Abort.
(* Here we define a correct [leb]. Usually I like to put this reference module
at the top of this file. *)
Module ref.
  Fixpoint leb (n m : nat) : bool :=
    match n with
    | O => true
    | S n' =>
        match m with
        | O => false
        | S m' => leb n' m'
        end
    end.
End ref.

Goal True.

  (* We define a tactic to try discharge the equivalence proof. A strong tactic
  can automatically decide a lot of correct answers. *)
  Ltac t1 := intros n; induction n; intros; eauto.
  (* We try to show [hw.leb] is equivalent to [ref.leb] by using the tactic
  [t1]. If it fails, an error will be generated. *)
  test (verify (forall n m, hw.leb n m = ref.leb n m) by t1).
  (* You can develop this tactic interactively by changing [verify] to
  [verify']. Then it will ask you to do the proof. I usually do this proof
  against the homework solution, and then turn that into a tactic. *)
  (* We can also add a falsifier, which checks if the student gets the answer
  wrong. If an error is emitted in a falsifier, or the answer uses disallowed
  axioms when we specify [ass], the student gets zero point immediately.
  Otherwise, we are not sure, so the grader either falls back to manual grading
  or tries the next verifier/falsifier if any. *)
  (* Falsifier is generally useful in checking if the students simply leave the
  exercise blank. *)
  falsifier.
  ass.
  Print Assumptions hw.leb.
  nl.

  (* Sometimes an exercise depends on the previous exercise, in that this
  exercise becomes meaningless if the previous one is wrong. For example,
  [leb_refl] depends on the correct definition of [leb]. If the student somehow
  defines [leb] as equal, they can still prove this theorem, but it may make
  this proof trivial or meaningless. In this case, we use the [dependencies]
  option. *)
  exc "leb_refl".
  pt 2.
  (* If the answer is correct but the exercises it depends on are wrong, turn
  this into a manual grade item. *)
  print_manual_grade mg_leb_refl.
  (* A list of exercises it depends on. Comma-separated. *)
  dependencies "leb".
  check_type @hw.leb_refl (forall n : nat, hw.leb n n = true).
  (* For a manual grade item (i.e. that has the [print_manual_grade] clause),
  complete verifier is not available, and the remainder of this exercise block
  is a falsifier if we do not specify explicitly. Here we use a sound verifier
  and a sound falsifier with the same body to emulate a complete verifier. This
  is not ideal. I would like to add a command for specifying complete verifier
  but haven't got the chance to do it yet (note the default should still be
  falsifier for manual grade items). *)
  verifier.
  ass.
  Print Assumptions hw.leb_refl.
  nl.
  falsifier.
  ass.
  Print Assumptions hw.leb_refl.
  nl.

  (* As an example to illustrate more about verifier/falsifier, the next
  exercise asks the students to fill in the statement and also prove it. *)
  exc "leb_succ".
  pt 3.
  print_manual_grade mg_leb_succ.
  dependencies "leb".
  (* Let's define a verifier. *)
  verifier.
  (* [test_type] is similar to [check_type], but it prints out an error if the
  types don't match. *)
  test_type @hw.leb_succ (forall n, hw.leb n (S n) = true).
  (* Don't forget to check if they finish the proof. *)
  ass.
  Print Assumptions hw.leb_succ.
  (* Of course this is not very robust as there are different ways to write down
  the statement, e.g., [forall n, true = hw.leb n (S n)]. We can have a better
  verifier. *)
  verifier.
  let P := type of hw.leb_succ in
  test (verify (P <-> (forall n, hw.leb n (S n) = true)) by (split; intros; eauto)).
  ass.
  Print Assumptions hw.leb_succ.
  (* Now let's define a falsifier to check if the students leave the question
  blank. *)
  falsifier.
  (* [refuse] takes a tactic and an error message. If the tactic succeeds, it
  prints out the error message. In this case, if the type of [leb_succ] is [False],
  we know the student has not done this exercise at all, so we give them a zero
  immediately. As a side note, we need to use [idtac] here to get around some
  stupid restriction in Ltac. *)
  refuse (idtac; check_type @hw.leb_succ False) "Blank answer".
  (* We can also print out the assumptions and check if they finish the proof in
  a falsifier. But we don't do it here because we would like to give them
  partial credits. *)
  nl.

  (* For exercises like [leb_succ], we may want to separate the score into two
  parts: the statement and the proof. We illustrate this approach using
  [leb_double]. DO NOT use [leb_double] as the name here, as latter exercises
  (including the next one checking the proof) can use it as an allowed axiom. *)
  exc "#leb_double_statement".
  (* We assign one point for the statement. *)
  pt 1.
  print_manual_grade mg_leb_double_statement.
  verifier.
  let P := type of hw.leb_double in
  test (verify (P <-> (forall n, hw.leb n (hw.double n) = true)) by (split; intros; eauto)).
  falsifier.
  refuse (idtac; check_type @hw.leb_double False) "Blank answer".
  nl.

  (* Now we check the proof. *)
  exc "leb_double".
  (* Let's say the proof is worth 2 points. *)
  pt 2.
  (* Sometimes we simply want to give an answer zero if the dependencies are
  wrong. However, unfortunately, at the moment [dependencies] option has to be
  paired with [print_manual_grade], meaning that we may have to manually grade
  the answer anyway. I know how to fix it, but haven't got the chance to
  implement it yet. *)
  print_manual_grade mg_leb_double.
  dependencies "leb,#leb_double_statement".
  verifier.
  ass.
  Print Assumptions hw.leb_double.
  nl.
  falsifier.
  ass.
  Print Assumptions hw.leb_double.
  nl.

  (* Let's see one more simple example for partial credits. We can simply use
  sound verifier instead of complete verifier and declare that the exercise
  should be manually graded. In this case, if they finish the proof, they get
  full mark. Otherwise, we manually check their proof and give partial
  credits. *)
  exc "plus_1".
  pt 10.
  print_manual_grade mg_plus_1.
  verifier.
  ass.
  Print Assumptions hw.plus_1.
  nl.

Abort.
