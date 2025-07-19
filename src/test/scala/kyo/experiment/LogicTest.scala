package kyo.experiment

import kyo.*

class LogicTest extends Test:

    "and Test" - {
        val n: Int < Logic                        = Logic.range(0, 3)
        val constrained: Seq[Int < Logic] < Logic = Logic.distinct(Seq.fill(2)(n))

        "direct" in run {

            val res: Chunk[Seq[Int]] = direct(constrained.now.map(_.now)).handle(Logic.run, _.eval)

            val checks = Seq(
                assert(res.forall(seq => seq.distinct == seq)),
                assert(res.size == 6)
            )

            succeed

        }

        "and" in run {
            val res = constrained.map(seq => Logic.andSeq(seq)).handle(
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
