package kyo.experiment

import kyo.*

class LogicTest extends Test:

    "and Test" - {

        def constrained(n: Int, m: Int): Seq[Int < Logic] < Logic =
            val vals: Int < Logic = Logic.range(0, m)
            Logic.distinct(Seq.fill(n)(vals))

        "direct" in run {

            val res: Chunk[Seq[Int]] = direct(constrained(2, 3).now.map(_.now)).handle(Logic.run, _.eval)

            val checks = Seq(
                assert(res.forall(seq => seq.distinct == seq)),
                assert(res.size == 6)
            )

            succeed

        }

        "and 2, 3" in run {
            val res = constrained(2,3).map(seq => Logic.andSeq(seq)).handle(
                Logic.run,
                _.eval
            )
            res.foreach(println)
            val checks = Seq(
                assert(res.size == 6)
            )

            succeed
        }

        "and 3, 3" in run {
            val res = constrained(3, 3).map(seq => Logic.andSeq(seq)).handle(
                Logic.run,
                _.eval
            )
            res.foreach(println)
            val checks = Seq(
                assert(res.size == 6)
            )

            succeed
        }
    }
end LogicTest
