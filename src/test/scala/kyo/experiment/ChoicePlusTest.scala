package experiment

import kyo.*
import kyo.experiment.ChoiceDistinct

opaque type Board = Seq[Int]

object Board:

    def queens(n: Int): Board < ChoiceDistinct =
        case class Position(row: Int, col: Int):
            def diag1: Int = row - col
            def diag2: Int = row + col

        val allPositions: Seq[Position < ChoiceDistinct] =
            (0 until n).map: row =>
                ChoiceDistinct.evalSeq(0 until n).map: col =>
                    Position(row, col)

        extension [A](seq: Seq[A < ChoiceDistinct] < ChoiceDistinct)
            def distinctOn[B](f: A => B): Seq[A < ChoiceDistinct] < ChoiceDistinct =
                seq.map: seq =>
                    ChoiceDistinct.distinctWith(seq)(f)
        end extension

        val constrained: Seq[Position < ChoiceDistinct] < ChoiceDistinct =
            allPositions
                .distinctOn(_.row) // true by construction
                .distinctOn(_.col)
                .distinctOn(_.diag1)
                .distinctOn(_.diag2)

        direct:
            val c = constrained.now
            c.map(_.now.col)
    end queens

end Board

@main def main =
    Stream.init(ChoiceDistinct.run(Board.queens(8)))
        .tap(board => println(board))
        .fold(0)((count, _) => count + 1)
        .map(count => println(s"Total solutions: $count"))
        .eval
