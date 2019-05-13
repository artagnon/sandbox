module Play

open FStar.Mul

val factorial : x:nat -> Tot nat
let rec factorial n =
  if n = 0 then 1 else n * (factorial (n - 1))

val length: list 'a -> Tot nat
let rec length l = match l with
  | [] -> 0
  | _ :: tl -> 1 + length tl

let rec append l1 l2 = match l1 with
  | [] -> l2
  | hd :: tl -> hd :: append tl l2

let rec sum_rec (n:nat) = if n > 0 then n + sum_rec (n-1) else 0

let sum_tot (n:nat) = ((n+1) * n) / 2

let rec sum_rec_correct (n:nat) : Lemma (sum_rec n = sum_tot n) =
  admit() (* replace this admit by a real proof *)

open FStar.Seq

val index' : ns:seq nat -> i:nat{i < length ns} -> nat
let index' ns i =
  let f (ns:seq nat) : nat =
      index ns i in
  f ns

// TypeScript lets you write JavaScript the way you really want to.
// TypeScript is a typed superset of JavaScript that compiles to plain JavaScript.