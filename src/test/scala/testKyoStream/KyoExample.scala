package testKyoStream

import kyo.*

val numberStream: Stream[Int, Any] = Stream.range(1, 11).rechunk(1)

//Async & Sync =:= Async
val processedStream: Stream[Unit, Async] = numberStream
    .map(x => x * 2)
    .map(x => Async.sleep(300.millis) *> x)
    .filter(_ % 4 == 0)
    .map(x => Console.printLine(s"Processing: $x"))

val timedStream: Stream[Unit, Async] = Stream.awakeEvery(1.second)
    .take(5)
    .map(duration => Console.printLine(s"Tick: ${duration.toSeconds}s"))

val combined = timedStream.merge(processedStream)

object TestStreamKyo extends KyoApp:
    run:
        combined.discard

extension (stream: Stream.type)
    def awakeEvery(duration: Duration): Stream[Duration, Async] =
        Stream:
            Clock.use: clock =>
                clock.nowMonotonic.map: start =>
                    Loop.indexed: i =>
                        clock.nowMonotonic.map: now =>
                            val d     = duration * (i + 1)
                            val delay = start + d - now
                            Async.sleep(delay) *> Emit.valueWith(Chunk(d))(Loop.continue)
end extension
