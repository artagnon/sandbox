From Coq Require Import ssreflect ssrfun ssrbool.
From mathcomp Require Import ssrnat.

Theorem dbl_implies: forall P Q R : Prop, (P -> Q) -> (Q -> R) -> P -> R.
Proof.
  (* The => tactical moves variables from goals to context.
   * It is the opposite of the : tactical.
   * move is simply a placeholder and does nothing.
   * 'move => P Q R' is equivalent to 'intros P Q R' *)
  move => P Q R H0 H1 a.
  exact (H1 (H0 a)).
Qed.

Theorem rev_implies (P Q R : Prop): (Q -> R) -> (P -> Q) -> P -> R.
Proof.
  (* apply is combined with the => and : tacticals here
   * // is equivalent to 'try done'
   * /= is equivalent to simpl
   * //= is equivalent to 'simpl; try done' *)
  move => H1 H2; apply: (dbl_implies P Q R) => //.
Qed.

Section connectives.
  Locate "_ /\ _".
  Print and.

  Theorem conj (A B : Prop): A -> B -> A /\ B.
  Proof.
    constructor 1 => //.
  Qed.

  Theorem disjunct: forall A B : Prop, A -> A \/ B.
  Proof.
    (* by is a terminal tactical (finishes off the proof)
     * Equivalen to 'left; done.'*)
      by left.
  Qed.

  Locate "~ _".

  Theorem contra (A B : Prop): (A -> B) -> ~ B -> ~ A.
  Proof.
    (* Uses ssreflect views to generalize and apply.
     * Still somewhat mysterious *)
    move => H Hq /H /Hq => //.
  Qed.

  Theorem quant A (S T: A -> Prop):
    (exists a : A, S a) -> (forall x : A, S x -> T x) -> exists b : A, T b.
  Proof.
    (* Perform case analysis and push three variables into context.
     * Equivalent to 'case; move => a H1 H2.' *)
    case => a H1 H2.
    exists a.
    apply: H2 => //.
  Qed.
End connectives.

Section rewriting.
  Definition double {A} (f : A -> A) (x : A) := f (f x).
  Fixpoint nat_iter {A} (n : nat) (f : A -> A) (x : A) :=
    if n is S n' then f (nat_iter n' f x) else x.
  Lemma double2 {A} (x : A) f t: t = double f x -> double f t = nat_iter 4 f x.
  Proof.
    (* The -> tactical pops the top assumption and rewrites
     * the goal with this anonymous fact, left to right.
     * Works nicely, because the preceding => pushed an assumption *)
    move => -> //.
  Qed.

  Check addnC.
  Print commutative.

  Definition f x y := x + y.
  Lemma comm_eq: forall x y, x + y + (y + x) = f y x + f y x.
  Proof.
    (* rewrite /f is equivalent to 'unfold f'
     * rewrite -/f is equivalent to 'fold f' *)
    move => x y; rewrite /f; congr (_ + _). rewrite addnC => //.
  Qed.
End rewriting.
