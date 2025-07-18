package testFs2

import cats.effect.IO
import cats.effect.IOApp
import fs2.Stream
import scala.concurrent.duration.*

object FS2Example extends IOApp.Simple:

    def run: IO[Unit] =
        val numberStream = Stream.range(1, 11)

        val processedStream = numberStream
            .map(x => x * 2)
            .evalMap(x => IO.sleep(300.millis) *> IO.pure(x))
            .filter(_ % 4 == 0)
            .evalMap(x => IO.println(s"Processing: $x"))

        val timedStream = Stream.awakeEvery[IO](1.second)
            .take(5)
            .evalMap(duration => IO.println(s"Tick: ${duration.toSeconds}s"))

        // Combine streams and run them
        val combined = Stream(
            processedStream,
            timedStream
        ).parJoinUnbounded

        // Execute the stream
        combined.compile.drain
    end run
end FS2Example
