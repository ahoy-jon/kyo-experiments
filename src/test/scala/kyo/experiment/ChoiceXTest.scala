package experiment

import kyo.*
import kyo.experiment.Cell
import kyo.experiment.Logic
import scala.util.Try

opaque type Board = Seq[Int]

object Board:

    def queens(n: Int): Board < Logic =
        case class Position(row: Int, col: Int):
            def diag1: Int = row - col
            def diag2: Int = row + col

        val allPositions: Seq[Position < Logic] =
            (0 until n).map: row =>
                Logic.range(0, n).map: col =>
                    Position(row, col)

        extension [A](seq: Seq[A < Logic] < Logic)
            def distinctOn[B](f: A => B)(using canEqual: CanEqual[B, B]): Seq[A < Logic] < Logic =
                seq.map: seq =>
                    Logic.distinctWith(seq)(f)
        end extension

        val constrained: Seq[Position < Logic] < Logic =
            allPositions
                .distinctOn(_.row) // true by construction
                .distinctOn(_.col)
                .distinctOn(_.diag1)
                .distinctOn(_.diag2)

        direct(Logic.andSeq(constrained.now).now.map(_.col))

    end queens

end Board

@main def main() =
    Stream.init(Logic.run(Board.queens(8)))
        .tap(board => println(board))
        .fold(0)((count, _) => count + 1)
        .map(count => println(s"Total solutions: $count"))
        .eval

case class LatinSquare[F[_]](grid: Chunk[Chunk[F[Int]]]):
    def size: Int = grid.size
    def mapRows[S](f: Chunk[F[Int]] => Seq[F[Int]] < S): LatinSquare[F] < S =
        direct(LatinSquare(grid.map(x => Chunk.from(f(x).now))))

    def flip: LatinSquare[F] =
        val idx = Chunk.range(0, size)
        LatinSquare(idx.map(i => grid.map(_(i))))

    def mapCols[S](f: Chunk[F[Int]] => Seq[F[Int]] < S): LatinSquare[F] < S =
        flip.mapRows(f).map(_.flip)

    def traverse[G[_], S](f: F[Int] => G[Int] < S): LatinSquare[G] < S =
        direct(LatinSquare(grid.map(_.map(c => f(c).now))))
end LatinSquare

object LatinSquare:

    def solve(latinSquare: LatinSquare[Cell]): LatinSquare[Id] < Logic =

        def distinctCells[A](seq: Seq[Cell[A]])(using canEqual: CanEqual[A, A]): Seq[Cell[A]] < Logic =
            Logic.distinctWith(seq)(identity)

        val constrained: LatinSquare[Cell] < Logic =
            direct(latinSquare.mapRows(distinctCells).now.mapCols(distinctCells).now)

        constrained.map(_.traverse(identity))
    end solve

    enum ParseError:
        case Oups

    def parse(text: String): LatinSquare[Cell] < Abort[ParseError] =
        val rows = Chunk.from(text.stripMargin.trim.linesIterator)
        val size = rows.length

        val choice: Int < Logic = Logic.range(0, size).map(_ + 1)

        if !rows.forall(_.length == size) then
            Abort.fail(ParseError.Oups)
        else

            type F[A] = (A < Logic) < Abort[ParseError]

            def parseChar(c: Char): F[Int] = c match
                case '.' => Kyo.lift(choice)
                case c if c.isDigit =>
                    val v = c.asDigit
                    if v >= 1 && v <= size then Kyo.lift(v)
                    else Abort.fail(ParseError.Oups)
                case _ => Abort.fail(ParseError.Oups)

            val parsed: Chunk[Chunk[F[Int]]] = rows.map: line =>
                Chunk.from(line.map(parseChar))

            LatinSquare[F](parsed).traverse(identity)
        end if
    end parse

    extension (latinSquare: LatinSquare[?])
        def show: String =
            latinSquare.grid.map(_.map(i =>
                Try(i.asInstanceOf[Int < Any].eval).getOrElse(".")
            ).mkString("")).mkString("\n")
    end extension

end LatinSquare

object TestLatinSquare extends KyoApp:

    def solve(text: String): Any < Sync =
        LatinSquare.parse(text)
            .map(LatinSquare.solve)
            .map(solved => Console.printLine(solved.show) *> Console.printLine("-" * 5))
            .handle(Logic.run, Abort.run(_))

    run:
        solve("""1.3.
                 |.2..
                 |..4.
                 |....""")

    run:
        solve(
            """......789
              |234567891
              |...6...1.
              |456.89123
              |567891234
              |6....23..
              |789123456
              |8912.....
              |912345678"""
        )
end TestLatinSquare
