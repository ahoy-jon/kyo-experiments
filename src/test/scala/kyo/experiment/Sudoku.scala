package experiment

import kyo.*
import kyo.experiment.Cell
import kyo.experiment.Logic
import scala.util.Try

case class Sudoku[F[_]](grid: Chunk[Chunk[F[Int]]]):
    val size: Int    = grid.size
    val boxSize: Int = math.sqrt(size).toInt
    assert(boxSize * boxSize == size)

    // Maps over rows, applying a function that enforces constraints or transformations
    def mapRows[S](f: Chunk[F[Int]] => Seq[F[Int]] < S): Sudoku[F] < S =
        direct(Sudoku(grid.map(x => Chunk.from(f(x).now))))

    // Flips the grid (transposes rows and columns)
    def flip: Sudoku[F] =
        val idx = Chunk.range(0, size)
        Sudoku(idx.map(i => grid.map(_(i))))

    // Maps over columns by flipping, applying row map, and flipping back
    def mapCols[S](f: Chunk[F[Int]] => Seq[F[Int]] < S): Sudoku[F] < S =
        flip.mapRows(f).map(_.flip)

    // Maps over sub-boxes (for Sudoku constraints)
    def mapBoxes[S](f: Chunk[F[Int]] => Seq[F[Int]] < S): Sudoku[F] < S =
        val n = Chunk.range(0, size)

        direct:
            Sudoku(n.map(box =>
                val chunk = n.map(idx =>
                    val row: Int = boxSize * (box / boxSize) + idx / boxSize
                    val col: Int = boxSize * (box % boxSize) + idx % boxSize
                    grid(row)(col)
                )
                Chunk.from(f(chunk).now)
            ))

    end mapBoxes

    // Traverses the grid, applying a function to each cell
    def traverse[G[_], S](f: F[Int] => G[Int] < S): Sudoku[G] < S =
        direct(Sudoku(grid.map(_.map(c => f(c).now))))
end Sudoku

object Sudoku:

    // Solves the Sudoku by enforcing distinct values in rows, columns, and boxes
    def solve(sudoku: Sudoku[Cell]): Sudoku[Id] < Logic =

        import Logic.distinct

        val constrained: Sudoku[Cell] < Logic =
            direct(sudoku.mapRows(distinct).now.mapCols(distinct).now.mapBoxes(distinct).now)

        constrained.map(_.traverse(identity))
    end solve

    enum ParseError:
        case InvalidInput, InvalidSize

    // Parses a string representation into a Sudoku grid
    def parse(text: String): Sudoku[Cell] < Abort[ParseError] =
        val rows = Chunk.from(text.stripMargin.trim.linesIterator.toVector)
        val size = rows.length

        val choice: Int < Logic = Logic.range(0, size).map(_ + 1)

        if !rows.forall(_.length == size) then
            Abort.fail(ParseError.InvalidInput)
        else

            type F[A] = (A < Logic) < Abort[ParseError]

            def parseChar(c: Char): F[Int] = c match
                case '.' => Kyo.lift(choice)
                case c if c.isDigit =>
                    val v = c.asDigit
                    if v >= 1 && v <= size then Kyo.lift(v)
                    else Abort.fail(ParseError.InvalidInput)
                case _ => Abort.fail(ParseError.InvalidInput)

            val parsed: Chunk[Chunk[F[Int]]] = Chunk.from(rows.map: line =>
                Chunk.from(line.map(parseChar).toVector))

            Sudoku[F](parsed).traverse(identity)
        end if
    end parse

    extension (sudoku: Sudoku[?])
        // Displays the Sudoku grid as a string
        def show: String =
            sudoku.grid.map(_.map {
                case v: Int => v.toString
                case other  => Try(other.asInstanceOf[Int < Any].eval).getOrElse(".").toString
            }
                .mkString("")).mkString("\n")
    end extension

end Sudoku

object TestSudoku extends KyoApp:

    def solve(text: String): Any < Sync =
        Sudoku.parse(text)
            .map(Sudoku.solve)
            .map(solved => Console.printLine(solved.show) *> Console.printLine("-" * solved.size))
            .handle(Logic.run, Abort.run(_))

    run:
        solve("""...4
                |....
                |2..3
                |4.12""".stripMargin)

    run:
        solve(
            """53..7....
              |6..195...
              |.98....6.
              |8...6...3
              |4..8.3..1
              |7...2...6
              |.6....28.
              |...419..5
              |....8..79""".stripMargin
        )
end TestSudoku
