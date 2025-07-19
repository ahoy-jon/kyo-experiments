package kyo.experiment

import kyo.*
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

        type State
        def init(index: Int, value: B): Maybe[State]
        def continue(state: State)(index: Int, value: B): Maybe[State]

        def merge(base: State, left: State, right: State): Maybe[State]

        final def contramap[A](f: A => B): Constraint[A] < Logic = new ConstraintContramap(this, f)
    end Constraint

    object Constraint:
        private[kyo] class Identity extends Serializable

    class ConstraintContramap[-A, B](val constraint: Constraint[B], f: A => B) extends Constraint[A]:
        override type State = constraint.State
        override def init(index: Int, value: A): Maybe[State]                    = constraint.init(index, f(value))
        override def continue(state: State)(index: Int, value: A): Maybe[State]  = constraint.continue(state)(index, f(value))
        override def merge(base: State, left: State, right: State): Maybe[State] = constraint.merge(base, left, right)

    end ConstraintContramap

    class Distinct[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = Set[B]

        override def init(index: Int, value: B): Maybe[Set[B]] = Maybe(Set(value))
        override def continue(state: Set[B])(index: Int, value: B): Maybe[Set[B]] =
            if state.contains(value) then Maybe.Absent else Maybe(state + value)

        def check(value: B)(using Frame): Unit < Logic =
            ArrowEffect.suspend(Tag[Logic], Op.Continue(this, 0, value))

        override def merge(base: Set[B], left: Set[B], right: Set[B]): Maybe[Set[B]] =
            val intersection = left.intersect(right)
            if base == intersection then Maybe(left ++ right) else Maybe.Absent
    end Distinct

    object Distinct:
        def init[B](using frame: Frame, canEqual: CanEqual[B, B]): Distinct[B] < Logic = Effect.deferInline(new Distinct[B]())

    class Equal[B](using CanEqual[B, B]) extends Constraint[B]:
        type State = B

        override def init(index: Int, value: B): Maybe[B]               = Maybe(value)
        override def continue(state: B)(index: Int, value: B): Maybe[B] = if state == value then Maybe(state) else Maybe.Absent

        def check(value: B)(using frame: Frame): Unit < Logic =
            ArrowEffect.suspend(Tag[Logic], Op.Continue(this, 0, value))

        override def merge(base: B, left: B, right: B): Maybe[B] = if left == right then Maybe(left) else Maybe.Absent
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
            case Nil => Nil
            case x :: xs => and(x, andSeq(xs)).map({
                    case (x, xs) => x +: xs
                })

    def run[A, S](v: A < (Logic & S))(using Frame): Chunk[A] < S =

        type AllStates = Map[Any, Any]

        case class Poll[+B](value: Maybe[B], nexts: Chunk[Poll[B] < (Logic & S)]):
            def addNext[B1 >: B](next: (Poll[B1] < (Logic & S))*): Poll[B1] = copy(nexts = nexts ++ next)
        end Poll

        object Poll:
            def empty[B]: Poll[B] = Poll(Maybe.Absent, Chunk.empty)

        extension [X, S1](v: X < S1)
            def toPoll: Poll[X] < S1 = v.map(x => Poll(Maybe(x), Chunk.empty))

        extension [X, S1](v: X < (S1 & Logic))
            def castS: X < Logic = v.asInstanceOf

        case class CartesianProduct[L, R](
            lefts: Chunk[L],
            rights: Chunk[R],
            nextLefts: Chunk[Poll[L] < (S & Logic)],
            nextRights: Chunk[Poll[R] < (S & Logic)]
        ):
            def runLeft[C](v: Poll[L] < (S & Logic), f: (L, R) => Poll[C] < (S & Logic)): Poll[C] < S =
                loopPoll(Map.empty, v).map(pair =>
                    val poll = pair._2
                    poll.value match
                        case Maybe.Absent => copy(nextLefts = poll.nexts ++ nextLefts).run(f)
                        case Maybe.Present(left) =>
                            val continue      = copy(lefts = lefts :+ left, nextLefts = poll.nexts ++ nextLefts).run(f)
                            val applyOnRights = rights.map(right => f(left, right))
                            Poll(Maybe.Absent, applyOnRights :+ continue)
                    end match
                )

            def runRight[C](v: Poll[R] < (S & Logic), f: (L, R) => Poll[C] < (S & Logic)): Poll[C] < S =
                loopPoll(Map.empty, v).map(pair =>
                    val poll = pair._2
                    poll.value match
                        case Maybe.Absent => copy(nextRights = poll.nexts ++ nextRights).run(f)
                        case Maybe.Present(right) =>
                            val continue     = copy(rights = rights :+ right, nextRights = poll.nexts ++ nextRights).run(f)
                            val applyOnLefts = lefts.map(left => f(left, right))
                            Poll(Maybe.Absent, applyOnLefts :+ continue)
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

        def loopPoll[B](allStates: AllStates, v: Poll[B] < (Logic & S))(using Frame): (AllStates, Poll[B]) < S =
            ArrowEffect.handleLoop(Tag[Logic], allStates, v)(
                [C] =>
                    (input, allStates, cont) =>
                        (input match
                            case Op.Continue(constraint, index, value) =>
                                val nextState: Maybe[constraint.State] = allStates.get(constraint.id) match
                                    case Some(state) => constraint.continue(state.asInstanceOf)(index, value)
                                    case None        => constraint.init(index, value)

                                nextState match
                                    case Maybe.Absent         => Loop.continue(Map.empty, Kyo.lift(Poll.empty[B]))
                                    case Maybe.Present(state) => Loop.continue(allStates + (constraint.id -> state), cont(()))

                            case Op.Or(seq) =>
                                Loop.continue(
                                    allStates,
                                    Kyo.lift(Poll[B](
                                        Maybe.Absent,
                                        Chunk.from(seq).map(a => loopPoll(allStates, a.map(a => cont(Kyo.lift(a))).castS).map(_._2))
                                    ))
                                )

                            case Logic.Op.And(left, right) =>
                                CartesianProduct(
                                    Chunk.empty,
                                    Chunk.empty,
                                    Chunk(loopPoll(allStates, left.toPoll.castS).map(_._2)),
                                    Chunk(loopPoll(allStates, right.toPoll.castS).map(_._2))
                                ).run((l, r) => cont(Kyo.lift((l, r)))).map(x => Loop.continue(allStates, Kyo.lift(x)))
                    ),
                (state: AllStates, b: Poll[B]) => Kyo.lift((state, b))
            )

        end loopPoll

        def unfold[X](poll: Poll[X]): Chunk[X] < S =
            Kyo.foldLeft(poll.nexts)(poll.value.toChunk)({
                case (res, n) =>
                    loopPoll[X](Map.empty, n).map(x => unfold(x._2)).map(res ++ _)
            })

        loopPoll(Map.empty, v.toPoll).map(x => unfold(x._2))

    end run

end Logic

type Cell[A] = A < Logic

