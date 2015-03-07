package org.uqbar.kaleidoscope.parser


object Lexer
trait Lexer {

    val ProgramStart = "module{"
    val BlockStartToken = "{"
    val BlockEndToken = "}"
    

    val TabSize = 4 //how many spaces does a tab take

    def tokenize(text: String): String = {
        val lines = text.split('\n').toList.filterNot(_.forall(isWhiteChar))
        val processedLines = ProgramStart :: tokenizeLines(lines, List(0))
        processedLines.mkString("\n")
    }

    def tokenizeLines(lines: List[String], stack: List[Int]): List[String] = lines match {
        case List() => List.fill(stack.length) { BlockEndToken }
        case line :: rest => {
            (computeIndentation(line), stack) match {
                case (indentation, top :: stackRest) if indentation > top => {
                    BlockStartToken :: line :: tokenizeLines(rest, indentation :: stack)
                }
                case (indentation, top :: stackRest) if indentation == top =>
                    line :: tokenizeLines(rest, stack)
                case (indentation, top :: stackRest) if indentation < top => {
                    BlockEndToken :: tokenizeLines(lines, stackRest)
                }
                case _ => throw new IllegalStateException("Invalid Token... stopping lexer.")
            }
        }
    }


    private def computeIndentation(line: String): Int = {
        val whiteSpace = line takeWhile isWhiteChar
        (whiteSpace map {
            case ' ' => 1
            case '\t' => TabSize
        }).sum
    }

    private def isWhiteChar(ch: Char) = ch == ' ' || ch == '\t'
}