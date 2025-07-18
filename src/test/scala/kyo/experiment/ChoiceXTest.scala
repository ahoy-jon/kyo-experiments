package experiment

import kyo.*
import kyo.experiment.Cell
import kyo.experiment.ChoiceX
import kyo.kernel.Effect
import scala.util.Try

opaque type Board = Seq[Int]

object Board:

    def queens(n: Int): Board < ChoiceX =
        case class Position(row: Int, col: Int):
            def diag1: Int = row - col
            def diag2: Int = row + col

        val allPositions: Seq[Position < ChoiceX] =
            (0 until n).map: row =>
                Choice.evalSeq(0 until n).map: col =>
                    Position(row, col)

        extension [A](seq: Seq[A < ChoiceX] < ChoiceX)
            def distinctOn[B](f: A => B): Seq[A < ChoiceX] < ChoiceX =
                seq.map: seq =>
                    ChoiceX.distinctWith(seq)(f)
        end extension

        val constrained: Seq[Position < ChoiceX] < ChoiceX =
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

@main def main() =
    Stream.init(ChoiceX.run(Board.queens(8)))
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

    def solve(latinSquare: LatinSquare[Cell]): LatinSquare[Id] < ChoiceX =

        def distinctCells[A](seq: Seq[Cell[A]]): Seq[Cell[A]] < ChoiceX =
            ChoiceX.distinctWith(seq)(identity)

        val constrained: LatinSquare[Cell] < ChoiceX =
            direct(latinSquare.mapRows(distinctCells).now.mapCols(distinctCells).now)

        constrained.map(_.traverse(identity))
    end solve

    enum ParseError:
        case Oups

    def parse(text: String): LatinSquare[Cell] < Abort[ParseError] =
        val rows = Chunk.from(text.stripMargin.trim.linesIterator)
        val size = rows.length

        val choice: Int < Choice = Choice.evalSeq(1 to size)

        if !rows.forall(_.length == size) then
            Abort.fail(ParseError.Oups)
        else

            type F[A] = (A < Choice) < Abort[ParseError]

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
            .handle(ChoiceX.run, Abort.run(_))

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
