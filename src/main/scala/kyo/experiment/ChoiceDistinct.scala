package kyo.experiment

import kyo.*
import kyo.kernel.*

sealed trait ChoiceDistinct extends ArrowEffect[ChoiceDistinct.Op, Id]

object ChoiceDistinct:

    enum Op[+Out]:
        case Drop()                                         extends Op[Nothing]
        case Options[A](seq: Seq[A])                        extends Op[A]
        case AddDistinct(id: Distinct.Identity, value: Any) extends Op[Unit]
    end Op

    val tag = Tag[ChoiceDistinct]

    trait Distinct[B]:
        def add(value: B)(using Frame): Unit < ChoiceDistinct

    object Distinct:
        private[kyo] class Identity extends Serializable

        private[kyo] class DistinctOf[B](identity: Identity) extends Distinct[B]:
            override def add(value: B)(using Frame): Unit < ChoiceDistinct =
                ArrowEffect.suspend(tag, Op.AddDistinct(identity, value))

        inline def init[B](using inline frame: Frame): Distinct[B] < ChoiceDistinct =
            Effect.deferInline:
                val id = new Identity
                new DistinctOf[B](id)

    end Distinct

    def distinctWith[A, S, B](seq: Seq[A < (S & ChoiceDistinct)])(f: A => B)(using Frame): Seq[A < (S & ChoiceDistinct)] < ChoiceDistinct =

        Distinct.init[B].map: distinct =>

            def handle(v: A < (S & ChoiceDistinct)): A < (S & ChoiceDistinct) =
                v.map(a => distinct.add(f(a)) *> a)

            seq.map(handle)

    /** Introduces a non-deterministic choice over a variadic list of values.
      *
      * @param a
      *   * Zero or more candidate values to choose from.
      * @return
      *   A computation that represent multiple paths, one for each input value.
      * @example
      *   {{{Choice.eval(1, 2, 3)}}}
      */
    inline def eval[A](a: A*)(using inline frame: Frame): A < ChoiceDistinct =
        evalSeq(a)

    /** Introduces a non-deterministic choice over a sequence of values.
      *
      * @param seq
      *   A sequence of candidate values to choose from.
      * @return
      *   A computation that represent multiple paths, one for each input value.
      * @example
      *   {{{Choice.evalFromSeq(Seq("a", "b", "c"))}}}
      */
    inline def evalSeq[A](seq: Seq[A])(using inline frame: Frame): A < ChoiceDistinct =
        ArrowEffect.suspend(tag, Op.Options(seq))

    /** Evaluates a function for each value in a sequence, introducing multiple computation paths.
      *
      * @param seq
      *   The sequence of input values
      * @param f
      *   The function to apply to each value
      * @return
      *   A computation that represents multiple paths, one for each input value
      */
    inline def evalWith[A, B, S](seq: Seq[A])(inline f: A => B < S)(using inline frame: Frame): B < (ChoiceDistinct & S) =
        seq match
            case Seq(head) => f(head)
            case seq       => ArrowEffect.suspendWith[A](Tag[ChoiceDistinct], Op.Options(seq))(f)

    /** Conditionally introduces a failure branch in the computation.
      *
      * @param condition
      *   The condition to check
      * @return
      *   A computation that fails if the condition is true, otherwise continues
      */
    inline def dropIf(condition: Boolean)(using inline frame: Frame): Unit < ChoiceDistinct =
        if condition then drop
        else ()

    /** Introduces an immediate failure branch in the computation.
      *
      * @return
      *   A computation that always fails
      */
    inline def drop(using inline frame: Frame): Nothing < ChoiceDistinct =
        ArrowEffect.suspend(Tag[ChoiceDistinct], Op.Drop())

    /** Handles the Choice effect by collecting all possible outcomes.
      *
      * @param v
      *   The computation with Choice effect to handle
      * @return
      *   A computation that produces a sequence of all possible outcomes
      */
    def run[A, S](v: A < (ChoiceDistinct & S))(using Frame): Chunk[A] < S =
        def branch(state: Set[(Any, Any)], v: Chunk[A] < (S & ChoiceDistinct)): Chunk[A] < S =
            ArrowEffect.handleLoop(Tag[ChoiceDistinct], state, v)(
                [C] =>
                    (input, state, cont) =>
                        input match
                            case Op.Drop()                                                => Loop.done(Chunk.empty)
                            case Op.AddDistinct(id, value) if state.contains(id -> value) => Loop.done(Chunk.empty)
                            case Op.AddDistinct(id, value)                                => Loop.continue(state + (id -> value), cont(()))
                            case Op.Options(seq) =>
                                Kyo.foreachConcat(Chunk.from(seq))(a => branch(state, cont(a))).map(Loop.done(_))
            )

        branch(Set.empty, v.map(a => Chunk(a)))
    end run

end ChoiceDistinct
