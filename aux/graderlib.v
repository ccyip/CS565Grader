Require Export String.
Open Scope string.

Ltac start m := idtac "----" m.
Ltac nl := idtac "".
Ltac exc x := idtac "*>" x.
Ltac itm x := idtac "#>" x.
Ltac pt x := idtac "Possible points:" x.
Ltac ass := idtac "Assumptions:".
Ltac verifier := idtac "~Verifier:".
Ltac falsifier := idtac "~Falsifier:".
Ltac error x := idtac "Fail:" x.
Ltac error' := error "no comment".

Ltac option k v := idtac "-" k ":" v.
Ltac allowed_module_axioms := option "allowed_module_axioms".
Ltac allowed_axioms := option "allowed_axioms".
Ltac dependencies := option "dependencies".

Parameter MISSING: Type.

Ltac check_type A B :=
    match type of A with
    | context[MISSING] => gfail 1 "Type missing for" A
    | ?T => unify T B
    end.

Ltac print_manual_grade A :=
    match type of A with
    | prod ?T string =>
      match eval compute in A with
      | (?S, ?C) =>
        idtac "Score:"  S;
        match eval compute in C with
        | ""%string => idtac "Comment: None"
        | _ => idtac "Comment:" C
        end
      end
    | nat => print_manual_grade (A, "")
    | bool => print_manual_grade (A, "")
    | _ => idtac "Score: Ungraded";
          idtac "Comment: None"
    end.

Ltac print_local_option name A :=
    match type of A with
    | string =>
      match eval compute in A with
      | ""%string => idtac
      | ?C => option name C
      end
    | _ => idtac
    end.

Tactic Notation "test" tactic(t) :=
    first [ t | error' ].

Tactic Notation "refuse" tactic(t) string(s) :=
    tryif t then error s else idtac.

Tactic Notation "verify" constr(x) "by" tactic(t) :=
    assert_succeeds (assert x by t).

Tactic Notation "verify'" constr(x) "by" tactic(t) := assert x.

Ltac test_type A B :=
    first [ check_type A B | error' ].

Axiom bad : False.

Require Export local.
