package org.uqbar.kaleidoscope.parser

import scala.util.parsing.combinator._
import com.sun.org.apache.xerces.internal.impl.xpath.regex.RegexParser

object KaleidoscopeParser extends KaleidoscopeParser
trait KaleidoscopeParser extends RegexParsers {

  sealed trait KaleiNode
  sealed trait Statement extends KaleiNode
  sealed trait PrimitiveType extends KaleiNode
  sealed trait Operation extends KaleiNode

  case class KaleidoscopeProgram(list: List[KaleiNode]) extends KaleiNode
  case class AlgebraicOperation(operation: String) extends Operation
  case class BooleanOperation(operation: String) extends Operation
  case class IntNode(i: Int) extends PrimitiveType
  case class IdentifierNode(s: String) extends PrimitiveType
  case class DefineNode(name: String, arguments: ArgumentsNode, list: List[KaleiNode]) extends Statement
  case class ArgumentsNode(arguments: List[ArgumentNode]) extends Statement
  case class ArgumentNode(argument: String) extends Statement
  case class ExpressionNode(le: PrimitiveType, op: Operation, re: PrimitiveType) extends Statement
  case class While(e: ExpressionNode, b: List[KaleiNode]) extends Statement
  case class IfBlock(bool: ExpressionNode, smts: List[KaleiNode]) extends Statement
   case class IfElseBlock(bool: ExpressionNode, smts: List[KaleiNode], elsesmts: List[KaleiNode]) extends Statement
  
  val blockStart = "{"
  val blockEnd = "}"
  val programStart = "module{"
  override protected val whiteSpace = """(\s|#.*)+""".r

  protected lazy val identifier = "[a-zA-Z0-9_]+".r
  protected lazy val strIdentifier = "[a-zA-Z]+".r
  protected lazy val comp = "=="
  protected lazy val different = "!="
  protected lazy val minor = "<"
  
  
  protected lazy val primitiveType: Parser[PrimitiveType] = int | identifierNode
  protected lazy val int = "[0-9]+".r ^^ { i => IntNode(i.toInt) }
  protected lazy val identifierNode = (identifier | strIdentifier) ^^ IdentifierNode

  protected lazy val define = "def" ~> identifier ~ arguments ~ "{" ~ statement.* <~ "}" ^^ { case identifier ~ arguments ~ "{" ~ statement => DefineNode(identifier, arguments, statement) }	  
  protected lazy val argument = strIdentifier ^^ ArgumentNode
  protected lazy val arguments = "(" ~> repsep(argument, ',') <~ ")" ^^ { case argument => ArgumentsNode(argument) }

  protected lazy val operation = regex("""[+-/\*]""".r) ^^ AlgebraicOperation
  protected lazy val booleanOp = (minor | comp | different) ^^ BooleanOperation
  
  protected lazy val program = "module{" ~> define.* <~ "}" ^^ KaleidoscopeProgram
  protected lazy val booleanExpression = primitiveType ~ booleanOp ~ primitiveType ^^ { case le ~ op ~ re => ExpressionNode(le, op, re) }
  protected lazy val expression = primitiveType ~ operation ~ primitiveType ^^ { case le ~ op ~ re => ExpressionNode(le, op, re) }
  protected lazy val whileStatement: Parser[While] =
    ("while" ~> "(" ~> booleanExpression <~ ")") ~ "{" ~ statement.* <~ "}" ^^ { case e ~ "{" ~ b => While(e, b) }
  protected lazy val ifStatement = ifelseblock | ifblock 
  protected lazy val ifblock =  ifClosure <~ "}"  ^^ {case bool ~ stmts => IfBlock(bool, stmts)}
  protected lazy val ifelseblock = ifClosure ~ ("}" ~ "else" ~ "{" ~> statement.* <~ "}") ^^ {case bool ~ stmts ~ elsestmts  => IfElseBlock(bool, stmts, elsestmts)}
  protected lazy val ifClosure =  ("if" ~ "(" ~> booleanExpression <~ ")") ~ ("then" ~ "{" ~> statement.*)
  protected lazy val statement: Parser[KaleiNode] = int | operation | whileStatement | ifStatement

  
  def apply(input: String) = parseAll(program, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw ParseException(msg)
  }

}