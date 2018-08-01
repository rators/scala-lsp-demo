package parsers

import model._
import parseback._

trait StatementParser extends ExpressionParser {

  lazy val statement: Parser[Statement] =
    nestedStatement |
      ifElseStatement |
      whileStatement |
      printStatement |
      assignmentStatement |
      arrayAssignStatement |
      returnStatement

  lazy val nestedStatement: Parser[NestedStatement] = "{" ~> (statement *) <~ "}" ^^ {
    (_, nestedStmts) => NestedStatement(nestedStmts)
  }

  lazy val ifElseStatement: Parser[IfElseStatement] = ((((("if" ~ "(") ~> expression <~ ")") ~ statement) ~ statement) <~ "else" ~> statement) ^^ {
    case (_, pred, thenStmt, elseStmt) => IfElseStatement(pred, thenStmt, elseStmt)
  }

  lazy val whileStatement: Parser[WhileStatement] = (("while" ~ "(") ~> expression ~ (")" ~> statement)) ^^ {
    case (_, exp, stmt) => WhileStatement(exp, stmt)
  }

  lazy val printStatement: Parser[PrintStatement] = (("System.out.println" ~ "(") ~> expression <~ (")" ~ ";")) ^^ {
    case (_, exp) => PrintStatement(exp)
  }

  lazy val assignmentStatement: Parser[AssignStatement] = (ID ~ ("=" ~> expression <~ ";")) ^^ {
    case (_, id, value) => AssignStatement(id, value)
  }

  lazy val arrayAssignStatement: Parser[ArrayAssignStatement] =
    ((ID <~ "[") ~ expression ~ (("]" ~ "=") ~> expression <~ ";")) ^^ {
      case (_, id, index, value) => ArrayAssignStatement(id, index, value)
    }

  lazy val returnStatement: Parser[ReturnStatement] = (("return" ~> expression) <~ ";") ^^ {
    case (_, expression) => ReturnStatement(expression)
  }

}
