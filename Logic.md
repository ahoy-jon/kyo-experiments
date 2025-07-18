# Logic Exploration in Kyo

## Overview

The exploration of logic in Kyo began while addressing issues in `AsyncShift` and debugging mutable builders in `direct(seq.map(x => f(x).now))`. These issues led to sequences that violated map invariants.

## Problem: Combinatorial Explosion

The primary challenge is combinatorial explosion. When using `Kyo.foreach` over a sequence of size `n` with `m` choices per element, the number of possibilities `u(n)` is defined as:

- `u(0) = 0`
- `u(n + 1) = m × (1 + u(n))`

This results in:

- `u(n) = m(mⁿ - 1)/(m - 1)`
- Approximately `u(n) ≈ mⁿ` for large `n`

The input space is `n * m`, but without early termination, the exploration space grows exponentially, making it impractical for large `n`.

### Monadic Explosion

The discrepancy between the input space (`n * m`) and the search space (`mⁿ`) arises from the monadic nature of the computation. The actual search space exceeds the theoretical search space by a factor of `m^(n-1)`.

Here's the relevant code for `ChoiceX.foreach`:

```scala
object ChoiceX:
    def foreach[A, B, S](lst: List[A])(f: A => B < (Choice & S))(using frame: Frame): List[B] < (S & Choice) =
        Kyo.foreach(lst)(x => Choice.run(f(x))).map: lst =>
            Kyo.foreach(lst)(x => Choice.evalSeq(x))
```

## Measuring Search Space

The following code snippets measure the evaluation and search spaces:

```scala
import kyo.*
import kyo.experiment.ChoiceX

def countEvalSpace(n: Int, m: Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq = Seq.fill(n)(Choice.evalSeq(0 until m) *> counter.increment)
        direct(seq.foreach(_.now)).handleChoice *> counter.get

def countSearchSpace(n: Int, m: Int): Long < Sync =
    val seq = Seq.fill(n)(Choice.evalSeq(0 until m))
    seq.foldLeft(1L)((n, i) => n * Choice.run(i).eval.size)

def countChoiceForeach(n: Int, m: Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq = Seq.fill(n)(Choice.evalSeq(0 until m) *> counter.increment)
        ChoiceX.foreach(seq.toList)(identity).handleChoice *> counter.get

def report(n: Int, m: Int) =
    direct:
        val count1 = countEvalSpace(n, m).now
        val count2 = countSearchSpace(n, m).now
        val count3 = countChoiceForeach(n, m).now
        Console.printLine(s"n:$n, m:$m, eval:$count1, search:$count2, staged:$count3").now

object CounterChoice extends KyoApp:
    run(report(2, 3))
    run(report(5, 5))
```

**Output:**

```
n:2, m:3, eval:12, search:9, staged:6
n:5, m:5, eval:3905, search:3125, staged:25
```

## Source of Explosion

The combinatorial explosion stems from the structure of combining exploration:

```scala
def zip[A1, A2, S](v1: A1 < S, v2: A2 < S)(using Frame): (A1, A2) < S =
    v1.map(t1 => v2.map(t2 => (t1, t2)))
```

Each multi-shot evaluation in `Kyo.foreach` evaluates the rest of the list, adding an extra `1/m` to the search space. Staged exploration with a lazy approach could mitigate this by evaluating only `n*m` without changing the user-intended semantics.

## Optimization Strategies

There are two primary approaches to address this issue:

1. **Reducing the Search Space**: Minimize the number of possibilities explored.
2. **Optimizing the Search Walk**: Improve the efficiency of exploring the necessary possibilities.

### Reducing the Search Space

One way to reduce search space is to prune early branches:

```scala
def queens(n: Int, row: Int = 0): Board < Choice =
    if row == n then b
    else
        Choice.evalWith(0 until n): col =>
            Choice.dropIf(!b.safe(row, col)).andThen:
                (b :+ col).queens(n, row + 1)
```

Since we cannot reason about ungrounded variables and their relationships (arc consistency), the most reachable tool compatible with monadic evaluation is "monotonic relational constraints".

#### Monotonic Constraints

**Example in Prolog:**

```prolog
:- use_module(library(clpfd)).

% N-Queens solver: Places N queens on an NxN board
queens(N, Queens) :-
    length(Queens, N),           % Queens is a list of N column positions
    Queens ins 1..N,             % Each queen's column is in 1..N
    safe_queens(Queens),         % Enforce monotonic constraints
    label(Queens).               % Assign values to find a solution

% Ensure queens do not attack each other (monotonic constraints)
safe_queens([]).
safe_queens([Q|Qs]) :-
    safe_queens(Qs, Q, 1),       % Check Q against all subsequent queens
    safe_queens(Qs).

% Check that queen at position Q in row I does not attack others
safe_queens([], _, _).
safe_queens([Q1|Qs], Q, I) :-
    Q #\= Q1,                    % Different columns
    abs(Q - Q1) #\= I,           % No diagonal attacks
    I1 is I + 1,
    safe_queens(Qs, Q, I1).

% Example query: ?- queens(4, Qs).
```

In this Prolog example:

- **Monotonic Constraints**: The constraints are monotonic because once satisfied for a partial assignment, they remain satisfied as more queens are placed.

**In Kyo**, we could abstract this as:

```scala
trait MonotonicConstraint[-B]:
    type State
    def init(index: Int, value: B): Maybe[State]
    def continue(state: State)(index: Int, value: B): Maybe[State]
    // def contramap[A](f: A => B): MonotonicConstraint[A]

def monotonic[A, B, S](seq: Seq[A < S])(f: A => B)(constraint: MonotonicConstraint[B]): Seq[A < (Choice & S)] < ChoiceX
```

This technique is used when solving N-Queen and Latin-Square to define distinct:

```scala
class Distinct[B] extends MonotonicConstraint[B] {
  type State = Set[B]

  def init(index: Int, value: B): Maybe[Set[B]] = Maybe(Set(value))

  def continue(state: Set[B])(index: Int, value: B): Maybe[Set[B]] =
    if state.contains(value) then
      Maybe.Absent
    else
      Maybe(state + value)
}
```

### other approaches to reduce search space:

Constraint Propagation ? However, constraint propagation over generation **would not**:
- be easily feasible (abstract the concept of variable, while we are manipulating values) 
- reduce search space. However, that would optimized what to search in streaming mode.

Monotonic constraints usually check as soon as possible.

Arc consistency is not possible without global reasoning over variable relationships, 
which is complicated when working with values


## Optimizing search

Curently the main problem with search optimisation is the absence of true `or` and `and`.
```scala
def or[A](a: (A < Choice)*):A < Choice //Close to Choice.eval
def orSeq[A](a: Seq[A < Choice]): A < Choice

def and[A, B](a: A < Choice, b: B < Choice): (A, B) < Choice //Close to Kyo.zip
def andSeq[A](a: Seq[A < Choice]):Seq[A] < Choice
```

Currently the search is deep-first, with some reordering with runStream, 
however since the flatMap is interleaved in the construction of the Logic problem, it's very difficult to optimise the search of solutions. 




