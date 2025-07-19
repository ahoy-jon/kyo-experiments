package kyo

abstract class Test extends org.scalatest.freespec.AsyncFreeSpec
    with org.scalatest.NonImplicitAssertions
    with kyo.internal.BaseKyoCoreTest:

    type Assertion = org.scalatest.Assertion

    def assertionSuccess = succeed

    def assertionFailure(msg: String) = fail(msg)

    override given executionContext: scala.concurrent.ExecutionContext = kyo.kernel.Platform.executionContext
end Test
