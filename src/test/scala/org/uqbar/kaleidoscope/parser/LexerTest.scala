package org.uqbar.kaleidoscope.parser

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class LexerTest extends FreeSpec with Matchers with Lexer {

  "SExpression Parser" - {

    "should tokenize" - {

      "simple expression" in {

        tokenize("""def fib(x)
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
