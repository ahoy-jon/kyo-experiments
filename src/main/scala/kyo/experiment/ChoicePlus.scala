package kyo.experiment

import kyo.*
import kyo.Tag
import kyo.debug.Debug
import kyo.kernel.*

sealed trait ChoicePlus extends ArrowEffect[ChoicePlus.Op, Id]

object ChoicePlus:

    enum Op[+Res]:
        case Drop()                                          extends Op[Nothing]
        case Options[A](seq: Seq[A])                         extends Op[A]
        case AddDistinct[B](id: Distinct.Identity, value: B) extends Op[Unit]
    end Op

    trait Distinct[B]:
        def add(value: B)(using Frame): Unit < ChoicePlus

    object Distinct:
        private[kyo] class Identity extends Serializable

        private[kyo] class DistinctOf[B](identity: Identity) extends Distinct[B]:
            override def add(value: B)(using Frame): Unit < ChoicePlus =
                ArrowEffect.suspend(Tag[ChoicePlus], Op.AddDistinct(identity, value))

        inline def init[B](using inline frame: Frame): Distinct[B] < ChoicePlus =
            Effect.deferInline:
                val id = new Identity
                new DistinctOf[B](id)

    end Distinct

    def distinctWith[A, S, B](seq: Seq[A < (S & ChoicePlus)])(f: A => B)(using Frame): Seq[A < (S & ChoicePlus)] < ChoicePlus =

        Distinct.init[B].map: distinct =>

            def handle(v: A < (S & ChoicePlus)): A < (S & ChoicePlus) =
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
    inline def eval[A](a: A*)(using inline frame: Frame): A < ChoicePlus =
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
    inline def evalSeq[A](seq: Seq[A])(using inline frame: Frame): A < ChoicePlus =
        ArrowEffect.suspend(Tag[ChoicePlus], Op.Options(seq))

    /** Evaluates a function for each value in a sequence, introducing multiple computation paths.
      *
      * @param seq
      *   The sequence of input values
      * @param f
      *   The function to apply to each value
      * @return
      *   A computation that represents multiple paths, one for each input value
      */
    inline def evalWith[A, B, S](seq: Seq[A])(inline f: A => B < S)(using inline frame: Frame): B < (ChoicePlus & S) =
        seq match
            case Seq(head) => f(head)
            case seq       => ArrowEffect.suspendWith[A](Tag[ChoicePlus], Op.Options(seq))(f)

    /** Conditionally introduces a failure branch in the computation.
      *
      * @param condition
      *   The condition to check
      * @return
      *   A computation that fails if the condition is true, otherwise continues
      */
    inline def dropIf(condition: Boolean)(using inline frame: Frame): Unit < ChoicePlus =
        if condition then drop
        else ()

    /** Introduces an immediate failure branch in the computation.
      *
      * @return
      *   A computation that always fails
      */
    inline def drop(using inline frame: Frame): Nothing < ChoicePlus =
        ArrowEffect.suspend(Tag[ChoicePlus], Op.Drop())

    /** Handles the Choice effect by collecting all possible outcomes.
      *
      * @param v
      *   The computation with Choice effect to handle
      * @return
      *   A computation that produces a sequence of all possible outcomes
      */
    def run[A, S](v: A < (ChoicePlus & S))(using Frame): Chunk[A] < S =
        ???
    /*ArrowEffect.handle(Tag[ChoicePlus], v.map(Chunk[A](_))) {
[C] =>
  (input, cont) =>
    Kyo.foreach(Chunk.from(input))(v => ChoicePlus.run(cont(v))).map(_.flattenChunk.flattenChunk)
}*/

end ChoicePlus
