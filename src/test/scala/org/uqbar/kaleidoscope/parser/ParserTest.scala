package org.uqbar.kaleidoscope.parser

import org.uqbar.testing.ParserTest
import org.scalatest.FreeSpec

class KaleidoscopeParserTest extends FreeSpec with ParserTest[KaleidoscopeParser] with KaleidoscopeParser {

  "SExpression Parser" - {
    
    implicit val parser = program
    
    "should parse" - {

      "empty module" in {
        """module{
          }""" should beParsedTo(KaleidoscopeProgram(List()))
      }

      "empty def" in {
        """module{
          def fib()
          {
          }
          }""" should beParsedTo(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List()),List()))))
      }

      "simple expression" in {
        """module{
          #Define fib
          def fib(x)
          {
            3 + 4
          }
          }"""should beParsedTo(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List(ArgumentNode("x"))),List(IntNode(3), AlgebraicOperation("+"), IntNode(4))))))
      }
      
       "simple while expression" in {
        """module{
          #Define fib
          def fib(x)
          {
            while (3==3)
          {
              3 + 4
          }
          }
          }""" should beParsedTo(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List(ArgumentNode("x"))),List(While(ExpressionNode(IntNode(3),BooleanOperation("=="),IntNode(3)),List(IntNode(3), AlgebraicOperation("+"), IntNode(4))))))))
      }
       
       "simple if Expression" in {
        """module{
          #Define fib
          def fib(x)
          {
            if (3==3) then
          {
              1
          }
          }
          }""" should beParsedTo(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List(ArgumentNode("x"))),List(IfBlock(ExpressionNode(IntNode(3),BooleanOperation("=="),IntNode(3)),List(IntNode(1))))))))
       }
       
       "simple if/else Expression" in {
        """module{
          #Define fib
          def fib(x)
          {
            if (x<3) then
          {
              1
          }
          else 
          {
             2*4
          }
          }
          }""" should beParsedTo(KaleidoscopeProgram(List(DefineNode("fib",ArgumentsNode(List(ArgumentNode("x"))),List(IfElseBlock(ExpressionNode(IdentifierNode("x"),BooleanOperation("<"),IntNode(3)),List(IntNode(1)),List(IntNode(2), AlgebraicOperation("*"), IntNode(4))))))))
        
 
       }

    }

  }
}