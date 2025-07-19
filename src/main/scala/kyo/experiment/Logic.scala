package kyo.experiment

import kyo.*
import kyo.experiment.Logic.Equal
import kyo.experiment.Logic.Op
import kyo.kernel.*

sealed trait Logic extends ArrowEffect[Logic.Op, Id]

object Logic:

    enum Op[+Out]:
        case Continue[B](constraint: Constraint[B], index: Int, value: B)              extends Op[Unit]
        case Or[A, S](seq: Seq[A < (Logic & S)])                                       extends Op[A < S]
        case And[Left, Right, S](left: Left < (Logic & S), right: Right < (Logic & S)) extends Op[(Left, Right) < S]
    end Op

    abstract class Constraint[-B]:
        private[kyo] val id = new Constraint.Identity

        type State <: Constraint.State[?, State]
        def init(index: Int, value: B): Maybe[State]
        def continue(state: State)(index: Int, value: B): Maybe[State]

        def merge(base: State, left: State, right: State): Maybe[State]

        final def contramap[A](f: A => B): Constraint[A] < Logic = new ConstraintContramap(this, f)
    end Constraint

    object Constraint:
        trait State[Diff, S <: State[Diff, S]]:

            def diff(next: S): Maybe[Diff]
            def next(diff: Diff): Maybe[S]

            def merge(left: S, right: S): Maybe[S] =
                diff(left) match
                    case Maybe.Absent => Maybe.Absent
                    case Maybe.Present(_) => diff(right) match
                            case Maybe.Absent      => Maybe.Absent
                            case Maybe.Present(dR) => left.next(dR)

            def merge(right: S): Maybe[S] =
                diff(right) match
                    case Maybe.Absent        => Maybe.Absent
                    case Maybe.Present(diff) => next(diff)

        end State

        private[kyo] class Identity extends Serializable:
            override def toString: String = s"Id:${hashCode() % 100}"
    end Constraint

    class ConstraintContramap[-A, B](val constraint: Constraint[B], f: A => B) extends Constraint[A]:
        override type State = constraint.State
        override def init(index: Int, value: A): Maybe[State]                    = constraint.init(index, f(value))
        override def continue(state: State)(index: Int, value: A): Maybe[State]  = constraint.continue(state)(index, f(value))
        override def merge(base: State, left: State, right: State): Maybe[State] = constraint.merge(base, left, right)

    end ConstraintContramap

    class Distinct[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = Distinct.State[B]

        override def init(index: Int, value: B): Maybe[Distinct.State[B]]       = Maybe(Distinct.State(Set(value)))
        override def continue(state: State)(index: Int, value: B): Maybe[State] = state.check(value)

        def check(value: B)(using Frame): Unit < Logic =
            ArrowEffect.suspend(Tag[Logic], Op.Continue(this, 0, value))

        override def merge(base: State, left: State, right: State): Maybe[State] = base.merge(left, right)
    end Distinct

    object Distinct:
        class State[B](val set: Set[B])(using CanEqual[B, B]) extends Constraint.State[Set[B], State[B]]:

            override def merge(left: State[B], right: State[B]): Maybe[State[B]] =
                val intersection = (set ++ left.set).intersect(set ++ right.set)
                val res          = if set == intersection then Maybe(State(left.set ++ right.set)) else Maybe.Absent
                res
            end merge

            override def diff(next: State[B]): Maybe[Set[B]] =
                val intersection = set.intersect(next.set)
                if intersection.nonEmpty then Maybe.Absent
                else Maybe(next.set -- set)
            end diff

            override def next(diff: Set[B]): Maybe[State[B]] =
                val intersection = set.intersect(diff)
                if intersection.nonEmpty then Maybe.Absent
                else Maybe(State(set ++ diff))
            end next

            def check(value: B): Maybe[State[B]] =
                if set.contains(value) then Maybe.Absent else Maybe(State(set + value))

            override def toString: String = s"Distinct(${set.mkString(",")})"

        end State

        def init[B](using frame: Frame, canEqual: CanEqual[B, B]): Distinct[B] < Logic = Effect.deferInline(new Distinct[B]())
    end Distinct

    class Equal[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = Equal.State[B]

        def check(value: B)(using frame: Frame): Unit < Logic =
            ArrowEffect.suspend(Tag[Logic], Op.Continue(this, 0, value))

        override def init(index: Int, value: B): Maybe[Equal.State[B]] = Maybe(Equal.State(value))

        override def continue(state: Equal.State[B])(index: Int, value: B): Maybe[Equal.State[B]] =
            state.check(value)

        override def merge(base: Equal.State[B], left: Equal.State[B], right: Equal.State[B]): Maybe[Equal.State[B]] =
            base.merge(left, right)
    end Equal

    object Equal:
        class State[B](val value: B)(using CanEqual[B, B]) extends Constraint.State[Unit, State[B]]:

            override def merge(left: State[B], right: State[B]): Maybe[State[B]] =
                if value == left.value && left.value == right.value then Maybe(this) else Maybe.Absent

            override def diff(next: State[B]): Maybe[Unit] =
                if value == next.value then Maybe(()) else Maybe.Absent

            override def next(diff: Unit): Maybe[State[B]] = Maybe(this)

            def check(value: B): Maybe[State[B]] =
                if this.value == value then Maybe(this) else Maybe.Absent

        end State
    end Equal

    def distinctWith[A, S, B](seq: Seq[A < S])(f: A => B)(using Frame, CanEqual[B, B]): Seq[A < (S & Logic)] < Logic =
        Distinct.init[B].map: distinct =>
            def handle(v: A < (S & Logic)): A < (S & Logic) =
                v.map(a => distinct.check(f(a)) *> a)

            seq.map(handle)

    def distinct[A, S](seq: Seq[A < S])(using Frame, CanEqual[A, A]): Seq[A < (S & Logic)] < Logic =
        distinctWith(seq)(identity)

    def or[A](a: A < Logic*)(using Frame): (A < Logic)          = orSeq(a)
    def orSeq[A](seq: Seq[A < Logic])(using Frame): (A < Logic) = ArrowEffect.suspendWith(Tag[Logic], Logic.Op.Or(seq))(identity)

    def drop(using Frame): Nothing < Logic = or()

    def range(from: Int, until: Int)(using frame: Frame): Int < Logic =
        if from < until then or(from, range(from + 1, until)) else drop

    def and[A, B, S](left: A < (Logic & S), right: B < (Logic & S))(using Frame): (A, B) < (Logic & S) =
        ArrowEffect.suspendWith(Tag[Logic], Logic.Op.And(left, right))(identity)

    def andSeq[A, S](a: Seq[A < (Logic & S)])(using Frame): Seq[A] < (Logic & S) =
        a match
            case Seq()       => Nil
            case Seq(x, xs*) => and(x, andSeq(xs)).map((x, xs) => x +: xs)

    def run[A, S](v: A < (Logic & S))(using Frame): Chunk[A] < S =

        type AllStates = Map[Constraint.Identity, Constraint.State[?, ?]]

        enum Value[+B] derives CanEqual:
            case Empty extends Value[Nothing]
            case Pure(pureValue: B)
            case ValueAndStates(value: B, states: AllStates)

            def toMaybe: Maybe[B] = this match
                case Empty                => Maybe.Absent
                case Pure(x)              => Maybe(x)
                case ValueAndStates(x, _) => Maybe(x)
        end Value

        case class Poll[+B](value: Value[B], nexts: Chunk[Poll[B] < (Logic & S)])

        object Poll:
            def empty[B]: Poll[B] = Poll(Value.Empty, Chunk.empty)

        extension [X, S1](v: X < S1)
            def toPoll: Poll[X] < S1 = v.map(x => Poll(Value.Pure(x), Chunk.empty))

        extension [X, S1](v: X < (S1 & Logic))
            def castS: X < Logic = v.asInstanceOf

        def merge(base: AllStates, left: AllStates, right: AllStates): Maybe[AllStates] =
            val keys: Set[Constraint.Identity] = base.keySet ++ left.keySet ++ right.keySet

            def build(id: Constraint.Identity): Maybe[Constraint.State[?, ?]] =
                (base.get(id), left.get(id), right.get(id)) match
                    case (None, None, None)                    => Maybe.Absent
                    case (Some(base), None, None)              => Maybe(base)
                    case (None, Some(left), None)              => Maybe(left)
                    case (None, None, Some(right))             => Maybe(right)
                    case (Some(base), Some(left), None)        => base.merge(left.asInstanceOf)
                    case (Some(base), None, Some(right))       => base.merge(right.asInstanceOf)
                    case (None, Some(left), Some(right))       => left.merge(right.asInstanceOf)
                    case (Some(base), Some(left), Some(right)) => base.merge(left.asInstanceOf, right.asInstanceOf)

            end build

            keys.foldLeft(Maybe(Map.empty[Constraint.Identity, Constraint.State[?, ?]]))((maybeMap, id) =>
                for
                    map   <- maybeMap
                    state <- build(id)
                yield map + (id -> state)
            )
        end merge

        case class CartesianProduct[L, R](
            baseState: AllStates,
            lefts: Chunk[Value[L]],
            rights: Chunk[Value[R]],
            nextLefts: Chunk[Poll[L] < (S & Logic)],
            nextRights: Chunk[Poll[R] < (S & Logic)]
        ):
            def runLeft[C](v: Poll[L] < (S & Logic), f: (L, R) => Poll[C] < (S & Logic)): Poll[C] < S =
                loopPoll(baseState, v).map(poll =>
                    poll.value.toMaybe match
                        case Maybe.Absent => copy(nextLefts = poll.nexts ++ nextLefts).run(f)
                        case Maybe.Present(left) =>
                            val continue = copy(lefts = lefts :+ poll.value, nextLefts = poll.nexts ++ nextLefts).run(f)

                            val applyOnRights = rights.flatMap(right =>

                                val state = merge(
                                    baseState,
                                    poll.value match
                                        case Value.ValueAndStates(_, states) => states
                                        case _ =>
                                            baseState
                                    ,
                                    right match
                                        case Value.ValueAndStates(_, states) => states
                                        case _                               => baseState
                                )

                                state match
                                    case Maybe.Absent         => None
                                    case Maybe.Present(state) => Some(loopPoll(state, f(left, right.toMaybe.get)))
                            )

                            Poll(Value.Empty, applyOnRights :+ continue)
                    end match
                )

            def runRight[C](v: Poll[R] < (S & Logic), f: (L, R) => Poll[C] < (S & Logic)): Poll[C] < S =
                loopPoll(baseState, v).map(poll =>

                    poll.value.toMaybe match
                        case Maybe.Absent => copy(nextRights = poll.nexts ++ nextRights).run(f)
                        case Maybe.Present(right) =>
                            val continue = copy(rights = rights :+ poll.value, nextRights = poll.nexts ++ nextRights).run(f)

                            val applyOnLefts = lefts.flatMap(left =>

                                val state = merge(
                                    baseState,
                                    left match
                                        case Value.ValueAndStates(_, states) => states
                                        case _ =>
                                            baseState
                                    ,
                                    poll.value match
                                        case Value.ValueAndStates(_, states) => states
                                        case _ =>
                                            baseState
                                )

                                state match
                                    case Maybe.Absent         => None
                                    case Maybe.Present(state) => Some(loopPoll(state, f(left.toMaybe.get, right)))
                            )

                            Poll(Value.Empty, applyOnLefts :+ continue)
                    end match
                )

            def run[C](f: (L, R) => Poll[C] < (S & Logic)): Poll[C] < S =

                inline def goLeft: Poll[C] < S = nextLefts match
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
        end CartesianProduct

        def loopPoll[B](allStates: AllStates, v: Poll[B] < (Logic & S))(using Frame): Poll[B] < S =
            ArrowEffect.handleLoop(Tag[Logic], allStates, v)(
                [C] =>
                    (input, allStates, cont) =>
                        (input match
                            case Op.Continue(constraint, index, value) =>
                                val nextState: Maybe[constraint.State] = allStates.get(constraint.id) match
                                    case Some(state) => constraint.continue(state.asInstanceOf)(index, value)
                                    case None        => constraint.init(index, value)

                                nextState match
                                    case Maybe.Absent         => Loop.done(Poll.empty)
                                    case Maybe.Present(state) => Loop.continue(allStates + (constraint.id -> state), cont(()))

                            case Op.Or(seq) =>
                                Loop.done(Poll[B](
                                    Value.Empty,
                                    Chunk.from(seq).map(a => loopPoll(allStates, a.map(a => cont(Kyo.lift(a))).castS))
                                ))

                            case Logic.Op.And(left, right) =>
                                Loop.continue(
                                    allStates,
                                    CartesianProduct(
                                        allStates,
                                        Chunk.empty,
                                        Chunk.empty,
                                        Chunk(left.toPoll.castS),
                                        Chunk(right.toPoll.castS)
                                    ).run((l, r) => cont(Kyo.lift((l, r))))
                                )
                    ),
                (state: AllStates, b: Poll[B]) =>
                    val newValue: Value[B] = b.value match
                        case Value.Empty   => Value.Empty
                        case Value.Pure(x) => Value.ValueAndStates(x, state)
                        case Value.ValueAndStates(x, newState) =>
                            merge(state, state, newState) match
                                case Maybe.Absent         => Value.Empty
                                case Maybe.Present(state) => Value.ValueAndStates(x, state)

                    b.copy(value = newValue)
            )

        end loopPoll

        def unfold[X](poll: Poll[X]): Chunk[X] < S =
            val chunk: Chunk[X] = poll.value match
                case Value.Empty                         => Chunk.empty
                case Value.Pure(pureValue)               => Chunk(pureValue)
                case Value.ValueAndStates(value, states) => Chunk(value)

            Kyo.foreachConcat(poll.nexts)(next =>
                loopPoll(Map.empty, next).map(unfold)
            ).map(chunk ++ _)
        end unfold

        loopPoll(Map.empty, v.toPoll).map(unfold)

    end run

end Logic

type Cell[A] = A < Logic
