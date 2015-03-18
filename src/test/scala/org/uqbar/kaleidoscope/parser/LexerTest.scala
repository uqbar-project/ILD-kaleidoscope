package org.uqbar.kaleidoscope.parser

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class PreParserProcessorTest extends FreeSpec with Matchers with PreParserProcessor {

  "SExpression Parser" - {

    "should tokenize" - {

      "simple expression" in {

        changeContextBoundaries("""def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2)""") should be("""module{
def fib(x)
{
  if x < 3 then
{
    1
}
  else
{
    fib(x-1)+fib(x-2)
}
}
}""")
      }

    }
  }
}
