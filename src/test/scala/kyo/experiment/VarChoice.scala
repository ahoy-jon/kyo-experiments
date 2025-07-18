package experiment

import kyo.*
import kyo.experiment.ChoiceX

def countEvalSpace(n: Int, m: Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq = Seq.fill(n)(Choice.evalSeq(0 until m) *> counter.increment)
        direct(seq.foreach(_.now)).handleChoice *> counter.get

def countSearchSpace(n: Int, m: Int): Long < Sync =
    val seq = Seq.fill(n)(Choice.evalSeq(0 until m))
    seq.foldLeft(1L)((n, i) => n * Choice.run(i).eval.size)

def countChoiceForeach(n: Int, m: Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq = Seq.fill(n)(Choice.evalSeq(0 until m) *> counter.increment)
        ChoiceX.foreach(seq.toList)(identity).handleChoice *> counter.get

def countEvalChoiceXSpace(n: Int, m:Int): Long < Sync =
    LongAdder.initWith: counter =>
        val seq: Seq[Unit < (Choice & Sync)] = Seq.fill(n)(Choice.evalSeq(0 until m) *> counter.increment)

        ChoiceX.run(ChoiceX.andSeq(seq)) *> counter.get
        //direct(seq.foreach(_.now)).handleChoice *> counter.get

def report(n: Int, m: Int) =
    direct:
        val count1 = countEvalSpace(n, m).now
        val count2 = countSearchSpace(n, m).now
        val count3 = countChoiceForeach(n, m).now
        val count4 = countEvalChoiceXSpace(n,m).now
        Console.printLine(s"n:$n,m:$m,eval:$count1,search:$count2,staged:$count3,trueZipEval:$count4").now

object CounterChoice extends KyoApp:
    run(report(2, 3))
    run(report(5, 5))

end CounterChoice

val prg =
    direct:
        Console.printLine("Welcome").now
        Console.printLine("to Kyo!").now
