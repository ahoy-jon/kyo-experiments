package kyo.experiment

import kyo.*
import kyo.experiment
import kyo.experiment.ChoiceConstraint.Op
import kyo.kernel.*

sealed trait ChoiceConstraint extends ArrowEffect[ChoiceConstraint.Op, Id]

object ChoiceConstraint:
    enum Op[+Out]:
        case Continue[B](constraint: Constraint[B], index: Int, value: B) extends Op[Unit]
    end Op

    abstract class Constraint[-B]:
        private[kyo] val id = new Constraint.Identity

        type State
        def init(index: Int, value: B): Maybe[State]
        def continue(state: State)(index: Int, value: B): Maybe[State]
        final def contramap[A](f: A => B): Constraint[A] < ChoiceConstraint = new ConstraintContramap(this, f)
    end Constraint

    object Constraint:
        private[kyo] class Identity extends Serializable

    class ConstraintContramap[-A, B](val base: Constraint[B], f: A => B) extends Constraint[A]:
        override type State = base.State
        override def init(index: Int, value: A): Maybe[base.State]                        = base.init(index, f(value))
        override def continue(state: base.State)(index: Int, value: A): Maybe[base.State] = base.continue(state)(index, f(value))
    end ConstraintContramap

    class Distinct[B] private () extends Constraint[B]:
        type State = Set[B]

        override def init(index: Int, value: B): Maybe[Set[B]] = Maybe(Set(value))
        override def continue(state: Set[B])(index: Int, value: B): Maybe[Set[B]] =
            if state.contains(value) then Maybe.Absent else Maybe(state + value)

        def check(value: B)(using Frame): Unit < ChoiceConstraint =
            ArrowEffect.suspend(Tag[experiment.ChoiceConstraint], Op.Continue(this, 0, value))
    end Distinct

    object Distinct:
        def init[B](using frame: Frame): Distinct[B] < ChoiceConstraint = Effect.deferInline(new Distinct[B]())

    class Equal[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = B

        override def init(index: Int, value: B): Maybe[B]               = Maybe(value)
        override def continue(state: B)(index: Int, value: B): Maybe[B] = if state == value then Maybe(state) else Maybe.Absent

        def check(value: B)(using frame: Frame): Unit < ChoiceConstraint =
            ArrowEffect.suspend(Tag[experiment.ChoiceConstraint], Op.Continue(this, 0, value))
    end Equal

    def distinctWith[A, S, B](seq: Seq[A < S])(f: A => B)(using Frame): Seq[A < (S & ChoiceConstraint)] < ChoiceConstraint =
        Distinct.init[B].map: distinct =>
            def handle(v: A < (S & ChoiceConstraint)): A < (S & ChoiceConstraint) =
                v.map(a => distinct.check(f(a)) *> a)

            seq.map(handle)

    def distinct[A, S](seq: Seq[A < S])(using Frame): Seq[A < (S & ChoiceConstraint)] < ChoiceConstraint =
        distinctWith(seq)(identity)

    def run[A, S](v: A < (ChoiceConstraint & S))(using Frame): Maybe[A] < S =
        def branch(allStates: Map[Any, Any], v: Maybe[A] < (S & ChoiceConstraint)): Maybe[A] < S =
            ArrowEffect.handleLoop(Tag[ChoiceConstraint], allStates, v)(
                [C] =>
                    (input, allStates, cont) =>
                        input match
                            case Op.Continue(constraint, index, value) =>
                                val nextState: Maybe[constraint.State] = allStates.get(constraint.id) match
                                    case Some(state) => constraint.continue(state.asInstanceOf)(index, value)
                                    case None        => constraint.init(index, value)

                                nextState match
                                    case Maybe.Absent         => Loop.done(Maybe.Absent)
                                    case Maybe.Present(state) => Loop.continue(allStates + (constraint.id -> state), cont(()))
            )

        branch(Map.empty, v.map(a => Maybe(a)))
    end run

end ChoiceConstraint

sealed trait ChoiceJunction extends ArrowEffect[ChoiceJunction.Op, Id]

object ChoiceJunction:
    enum Op[+Out]:
        case Or[A](seq: Seq[A < ChoiceX])              extends Op[A]
        case And[A, B](a: A < ChoiceX, b: B < ChoiceX) extends Op[(A, B)]
        case AndSeq[A](seq: Seq[A < ChoiceX])          extends Op[Seq[A]]
    end Op
end ChoiceJunction

opaque type ChoiceX <: (Choice & ChoiceConstraint & ChoiceJunction) = Choice & ChoiceConstraint & ChoiceJunction

type Cell[A] = A < ChoiceX

object ChoiceX:

    def or[A](a: A < ChoiceX*)(using Frame): (A < ChoiceX)          = orSeq(a)
    def orSeq[A](seq: Seq[A < ChoiceX])(using Frame): (A < ChoiceX) = ArrowEffect.suspend(Tag[ChoiceJunction], ChoiceJunction.Op.Or(seq))

    def and[A, B](a: A < ChoiceX, b: B < ChoiceX)(using Frame): (A, B) < ChoiceX =
        ArrowEffect.suspend(Tag[ChoiceJunction], ChoiceJunction.Op.And(a, b))

    def andSeq[A](a: Seq[A < ChoiceX])(using Frame): Seq[A] < ChoiceX =
        ArrowEffect.suspend(Tag[ChoiceJunction], ChoiceJunction.Op.AndSeq(a))

    type Distinct[B] = ChoiceConstraint.Distinct[B]

    export ChoiceConstraint.distinctWith

    object Distinct:
        export ChoiceConstraint.Distinct.init

    def run[A, S](v: A < (ChoiceX & S))(using Frame): Chunk[A] < S =
        def branch(allStates: Map[Any, Any], v: Chunk[A] < (S & ChoiceX)): Chunk[A] < S =
            // ArrowEffect.handleLoop don't exist for two effects at the same time
            // need to redo
            ArrowEffect.handle(Tag[ChoiceConstraint], Tag[Choice], Tag[ChoiceJunction], v)(
                [C] =>
                    (input, cont) =>
                        (input match
                            case Op.Continue(constraint, index, value) =>
                                val nextState: Maybe[constraint.State] = allStates.get(constraint.id) match
                                    case Some(state) => constraint.continue(state.asInstanceOf)(index, value)
                                    case None        => constraint.init(index, value)

                                nextState match
                                    case Maybe.Absent         => Chunk.empty
                                    case Maybe.Present(state) => branch(allStates + (constraint.id -> state), cont(()))
                    ),
                [C] =>
                    (input, cont) =>
                        Kyo.foreachConcat(Chunk.from(input))(a => branch(allStates, cont(a))),
                [C] =>
                    (input, cont) =>
                        //optimize to avoid explosion

                        (input match
                            case ChoiceJunction.Op.Or(seq) =>
                                Kyo.foreachConcat(Chunk.from(seq))(c => branch(allStates, c.map(cont)))

                            case ChoiceJunction.Op.AndSeq(seq) =>
                                Kyo.foreach(seq)(identity).map(cont)

                            case ChoiceJunction.Op.And(a, b) =>
                                //still monadic, should pull the first result of a, keep it, then the first result of b, cont
                                //(1, 2, 3), (a, b, c)
                                // compute 1
                                // compute a
                                // cont (1, a)
                                // compute b
                                // cont (1, b)
                                // compute c
                                // cont (1, c)
                                // compute 2
                                // cont (2, a)
                                // cont (2, b)
                                // cont (2, c)
                                // compute 3
                                // cont (3, a)
                                // cont (3, b)
                                // cont (3, c)
                                Kyo.zip(a,b).map(cont)
                            )
            )

        branch(Map.empty, v.map(a => Chunk(a)))
    end run

    // unoptimized foreach
    def foreach[A, B, S](lst: List[A])(f: A => B < (Choice & S))(using frame: Frame): List[B] < (S & Choice) =
        Kyo.foreach(lst)(x => Choice.run(f(x))).map: lst =>
            Kyo.foreach(lst)(x => Choice.evalSeq(x))

end ChoiceX
