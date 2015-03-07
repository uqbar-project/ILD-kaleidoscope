package org.uqbar.kaleidoscope.parser

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class ParserTest extends FreeSpec with Matchers with KaleidoscopeParser {

  "SExpression Parser" - {

    "should parse" - {

      "empty module" in {
        val input = """module{
          }"""
        apply(input) should be(KaleidoscopeProgram(List()))
      }

      "empty def" in {
        val input = """module{
          def fib()
          {
          }
          }"""
        apply(input) should be(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List()),List()))))
      }

      "simple expression" in {
        val input = """module{
          #Define fib
          def fib(x)
          {
            3 + 4
          }
          }"""
        apply(input) should be(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List(ArgumentNode("x"))),List(IntNode(3), AlgebraicOperation("+"), IntNode(4))))))
      }
      
       "simple while expression" in {
        val input = """module{
          #Define fib
          def fib(x)
          {
            while (3==3)
          {
              3 + 4
          }
          }
          }"""
        apply(input) should be(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List(ArgumentNode("x"))),List(While(ExpressionNode(IntNode(3),BooleanOperation("=="),IntNode(3)),List(IntNode(3), AlgebraicOperation("+"), IntNode(4))))))))
      }

      "complex expression" in {
        val input: String = """module{      
# Compute the x'th fibonacci number.
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
# This expression will compute the 40th number.
fib(40)
}"""
        //parse(input) should be()

      }

    }

  }
}