package experiment

import kyo.*
import kyo.experiment.Logic

def countEvalSpace(n: Int, m: Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq = Seq.fill(n)(Choice.evalSeq(0 until m) *> counter.increment)
        direct(seq.foreach(_.now)).handleChoice *> counter.get

def countSearchSpace(n: Int, m: Int): Long < Sync =
    val seq = Seq.fill(n)(Logic.range(0, m))
    seq.foldLeft(1L)((n, i) => n * Logic.run(i).eval.size)

def countLogicForeach(n: Int, m: Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq = Seq.fill(n)(Logic.range(0, m) *> counter.increment)
        Kyo.foreach(seq)(v => Logic.run(v)) *> counter.get

def countEvalLogicXSpace(n: Int, m: Int): Long < Sync =
    LongAdder.initWith:
        counter =>
            val seq: Seq[Unit < (Logic & Sync)] = Seq.fill(n)(Logic.range(0, m) *> counter.increment)

            Logic.run(Logic.andSeq(seq)) *> counter.get
            // direct(seq.foreach(_.now)).handleLogic *> counter.get

def report(n: Int, m: Int) =
    direct:
        val count1 = countEvalSpace(n, m).now
        val count2 = countSearchSpace(n, m).now
        val count3 = countLogicForeach(n, m).now
        val count4 = countEvalLogicXSpace(n, m).now
        Console.printLine(s"n:$n,m:$m,choiceEval:$count1,searchSpace:$count2,staged:$count3,cartesianEval:$count4").now

object CounterLogic extends KyoApp:
    run(report(2, 3))
    run(report(5, 5))

end CounterLogic

val prg =
    direct:
        Console.printLine("Welcome").now
        Console.printLine("to Kyo!").now
