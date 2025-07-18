package kyo.experiment

import kyo.*
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

        def merge(base: State, left: State, right: State): Maybe[State]

        final def contramap[A](f: A => B): Constraint[A] < ChoiceConstraint = new ConstraintContramap(this, f)
    end Constraint

    object Constraint:
        private[kyo] class Identity extends Serializable

    class ConstraintContramap[-A, B](val constraint: Constraint[B], f: A => B) extends Constraint[A]:
        override type State = constraint.State
        override def init(index: Int, value: A): Maybe[State]                   = constraint.init(index, f(value))
        override def continue(state: State)(index: Int, value: A): Maybe[State] = constraint.continue(state)(index, f(value))

        override def merge(base: State, left: State, right: State): Maybe[State] = constraint.merge(base, left, right)

    end ConstraintContramap

    class Distinct[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = Set[B]

        override def init(index: Int, value: B): Maybe[Set[B]] = Maybe(Set(value))
        override def continue(state: Set[B])(index: Int, value: B): Maybe[Set[B]] =
            if state.contains(value) then Maybe.Absent else Maybe(state + value)

        def check(value: B)(using Frame): Unit < ChoiceConstraint =
            ArrowEffect.suspend(Tag[experiment.ChoiceConstraint], Op.Continue(this, 0, value))

        override def merge(base: Set[B], left: Set[B], right: Set[B]): Maybe[Set[B]] =
            val intersection = left.intersect(right)
            if base == intersection then Maybe(left ++ right) else Maybe.Absent
    end Distinct

    object Distinct:
        def init[B](using frame: Frame, canEqual: CanEqual[B, B]): Distinct[B] < ChoiceConstraint = Effect.deferInline(new Distinct[B]())

    class Equal[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = B

        override def init(index: Int, value: B): Maybe[B]               = Maybe(value)
        override def continue(state: B)(index: Int, value: B): Maybe[B] = if state == value then Maybe(state) else Maybe.Absent

        def check(value: B)(using frame: Frame): Unit < ChoiceConstraint =
            ArrowEffect.suspend(Tag[experiment.ChoiceConstraint], Op.Continue(this, 0, value))

        override def merge(base: B, left: B, right: B): Maybe[B] = if left == right then Maybe(left) else Maybe.Absent
    end Equal

    def distinctWith[A, S, B](seq: Seq[A < S])(f: A => B)(using Frame, CanEqual[B, B]): Seq[A < (S & ChoiceConstraint)] < ChoiceConstraint =
        Distinct.init[B].map: distinct =>
            def handle(v: A < (S & ChoiceConstraint)): A < (S & ChoiceConstraint) =
                v.map(a => distinct.check(f(a)) *> a)

            seq.map(handle)

    def distinct[A, S](seq: Seq[A < S])(using Frame, CanEqual[A, A]): Seq[A < (S & ChoiceConstraint)] < ChoiceConstraint =
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
        case Or[A](seq: Seq[A < ChoiceX])                                                  extends Op[A]
        case And[Left, Right, S](left: Left < (ChoiceX & S), right: Right < (ChoiceX & S)) extends Op[(Left, Right) < S]
        // case AndSeq[A](seq: Seq[A < ChoiceX])          extends Op[Seq[A]]
    end Op
end ChoiceJunction

opaque type ChoiceX <: (Choice & ChoiceConstraint & ChoiceJunction) = Choice & ChoiceConstraint & ChoiceJunction

type Cell[A] = A < ChoiceX

object ChoiceX:

    def or[A](a: A < ChoiceX*)(using Frame): (A < ChoiceX)          = orSeq(a)
    def orSeq[A](seq: Seq[A < ChoiceX])(using Frame): (A < ChoiceX) = ArrowEffect.suspend(Tag[ChoiceJunction], ChoiceJunction.Op.Or(seq))

    def and[A, B, S](left: A < (ChoiceX & S), right: B < (ChoiceX & S))(using Frame): (A, B) < (ChoiceX & S) =
        ArrowEffect.suspendWith(Tag[ChoiceJunction], ChoiceJunction.Op.And(left, right))(identity)

    def andSeq[A, S](a: Seq[A < (ChoiceX & S)])(using Frame): Seq[A] < (ChoiceX & S) =
        a match
            case Nil => Nil
            case x :: xs => and(x, andSeq(xs)).map({
                    case (x, xs) => x +: xs
                })

    type Distinct[B] = ChoiceConstraint.Distinct[B]

    export ChoiceConstraint.distinctWith

    object Distinct:
        export ChoiceConstraint.Distinct.init

    def run[A, S](v: A < (ChoiceX & S))(using Frame): Chunk[A] < S =

        type AllStates = Map[Any, Any]

        case class Poll[+B](value: Maybe[B], nexts: Chunk[Poll[B] < (ChoiceX & S)]):

            def addNext[B1 >: B](next: (Poll[B1] < (ChoiceX & S))*): Poll[B1] = copy(nexts = nexts ++ next)
        end Poll

        object Poll:
            def empty[B]: Poll[B] = Poll(Maybe.Absent, Chunk.empty)

        extension [X, S1](v: X < S1)
            def toPoll: Poll[X] < S1 = v.map(x => Poll(Maybe(x), Chunk.empty))

        extension [X, S1](v: X < (S1 & ChoiceX))
            def castS: X < ChoiceX = v.asInstanceOf

        case class TrueZip[L, R](
            lefts: Chunk[L],
            rights: Chunk[R],
            nextLefts: Chunk[Poll[L] < (S & ChoiceX)],
            nextRights: Chunk[Poll[R] < (S & ChoiceX)]
        ):
            def runLeft[C](v: Poll[L] < (S & ChoiceX), f: (L, R) => Poll[C] < (S & ChoiceX)): Poll[C] < (S & ChoiceX) =
                v.map(poll =>
                    poll.value match
                        case Maybe.Absent => copy(nextLefts = poll.nexts ++ nextLefts).run(f)
                        case Maybe.Present(left) =>
                            val continue      = copy(lefts = lefts :+ left, nextLefts = poll.nexts ++ nextLefts).run(f)
                            val applyOnRights = rights.map(right => f(left, right))
                            Poll(Maybe.Absent, applyOnRights :+ continue)
                )

            def runRight[C](v: Poll[R] < (S & ChoiceX), f: (L, R) => Poll[C] < (S & ChoiceX)): Poll[C] < (S & ChoiceX) =
                v.map(poll =>
                    poll.value match
                        case Maybe.Absent => copy(nextRights = poll.nexts ++ nextRights).run(f)
                        case Maybe.Present(right) =>
                            val continue     = copy(rights = rights :+ right, nextRights = poll.nexts ++ nextRights).run(f)
                            val applyOnLefts = lefts.map(left => f(left, right))
                            Poll(Maybe.Absent, applyOnLefts :+ continue)
                )

            def run[C](f: (L, R) => Poll[C] < (S & ChoiceX)): Poll[C] < (S & ChoiceX) =
                inline def goLeft = nextLefts match
                    case Chunk()       => Kyo.lift(Poll.empty[C])
                    case Chunk(x, xs*) => copy(nextLefts = Chunk.from(xs)).runLeft(x, f)

                inline def goRight = nextRights match
                    case Chunk()       => goLeft
                    case Chunk(x, xs*) => copy(nextRights = Chunk.from(xs)).runRight(x, f)

                if lefts.isEmpty then
                    goLeft
                else
                    goRight
                end if
            end run
        end TrueZip

        def loopPoll[B](allStates: AllStates, v: Poll[B] < (ChoiceX & S))(using Frame): Poll[B] < S =
            ArrowEffect.handle(Tag[ChoiceConstraint], Tag[Choice], Tag[ChoiceJunction], v)(
                [C] =>
                    (input, cont) =>
                        (input match
                            case Op.Continue(constraint, index, value) =>
                                val nextState: Maybe[constraint.State] = allStates.get(constraint.id) match
                                    case Some(state) => constraint.continue(state.asInstanceOf)(index, value)
                                    case None        => constraint.init(index, value)

                                nextState match
                                    case Maybe.Absent         => Poll.empty
                                    case Maybe.Present(state) => loopPoll(allStates + (constraint.id -> state), cont(()))
                    ),
                [C] =>
                    (input, cont) =>
                        Poll[B](Maybe.Absent, Chunk.from(input).map(a => loopPoll(allStates, cont(a)))),
                [C] =>
                    (input, cont) =>
                        (input match
                                case ChoiceJunction.Op.Or(seq) =>
                                    Poll[B](Maybe.Absent, Chunk.from(seq).map(a => loopPoll(allStates, a.map(cont))))

                                case ChoiceJunction.Op.And(left, right) =>
                                    TrueZip(
                                        Chunk.empty,
                                        Chunk.empty,
                                        Chunk(loopPoll(allStates, left.toPoll.castS)),
                                        Chunk(loopPoll(allStates, right.toPoll.castS))
                                    ).run((l, r) => cont(Kyo.lift((l, r))))

                                // still monadic, should pull the first result of a, keep it, then the first result of b, cont
                                // (1, 2, 3), (a, b, c)
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

                    )
            )
        end loopPoll

        def unfold[X](poll: Poll[X]): Chunk[X] < S =
            Kyo.foldLeft(poll.nexts)(poll.value.toChunk)({
                case (res, n) =>
                    loopPoll[X](Map.empty, n).map(unfold).map(res ++ _)
            })

        loopPoll(Map.empty, v.map(a => Poll(Maybe(a), Chunk.empty))).map(unfold)

    end run

    // unoptimized foreach
    def foreach[A, B, S](lst: List[A])(f: A => B < (Choice & S))(using frame: Frame): List[B] < (S & Choice) =
        Kyo.foreach(lst)(x => Choice.run(f(x))).map: lst =>
            Kyo.foreach(lst)(x => Choice.evalSeq(x))

end ChoiceX
