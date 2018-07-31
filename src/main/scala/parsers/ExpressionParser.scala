package parsers

import model._
import parseback._

trait ExpressionParser extends ParserBase {
  lazy val expression: Parser[Expression] = arrayAccessExpression |
    arrayLengthExpression |
    methodCallExpression |
    negExpression |
    notExpression |
    arrayInstantiationExpression |
    objectInstantiationExpression |
    binaryOperatorExpression |
    INT.map(IntLiteral) |
    BOOL.map(BooleanLiteral) |
    identifierExpression |
    thisExpression |
    parenExpression

  lazy val arrayLengthExpression: Parser[ArrayLengthExpression] =
    (expression <~ ("." ~ "length")) ^^ {
      (_, e) => ArrayLengthExpression(e)
    }

  lazy val arrayAccessExpression: Parser[ArrayAccessExpression] =
    (expression ~ ("[" ~> expression <~ "]")) ^^ {
      (_, a, i) => ArrayAccessExpression(a, i)
    }

  lazy val methodCallExpression: Parser[MethodCallExpression] =
    ((expression <~ ".") ~ (ID ~ methodArgumentList)) ^^ {
      (_, e, i, ma) => MethodCallExpression(e, i, ma)
    }

  lazy val notExpression: Parser[UnaryOperatorExpression] = ("!" ~> expression) ^^ {
    (_, e) => UnaryOperatorExpression(Not, e)
  }

  lazy val negExpression: Parser[UnaryOperatorExpression] = ("-" ~> expression) ^^ {
    (_, e) => UnaryOperatorExpression(Neg, e)
  }

  lazy val arrayInstantiationExpression: Parser[ArrayInstantiationExpression] =
    (("new" ~> "int") ~> ("[" ~> expression <~ "]")) ^^ {
      (_, e) => ArrayInstantiationExpression(e)
    }

  lazy val objectInstantiationExpression: Parser[ObjectInstantiationExpression] =
    ("new" ~> klassType <~ ("(" ~ ")")) ^^ {
      (_, e) => ObjectInstantiationExpression(e)
    }

  lazy val binaryOperatorExpression: Parser[BinaryOperatorExpression] = (expression ~ binaryOperator ~ expression) ^^ {
    (_, lh, op, rh) => BinaryOperatorExpression(lh, op, rh)
  }

  lazy val identifierExpression: Parser[IdentifierExpression] = ID.map(IdentifierExpression)

  lazy val thisExpression: Parser[ThisExpression.type] = "this" ^^^ ThisExpression

  lazy val parenExpression: Parser[ParenExpression] = ("(" ~> expression <~ ")") ^^ {
    (_, e) => ParenExpression(e)
  }

  lazy val binaryOperator: Parser[BinaryOperator] =
    """\+|-|\*|&&|<""".r ^^ {
      case (_, "+") => Plus
      case (_, "-") => Subtract
      case (_, "*") => Multiply
      case (_, "<") => LessThan
      case (_, "&&") => And
    }

  lazy val methodArgumentList: Parser[List[Expression]] = ("(" ~> (expression ~ (("," ~> expression) *) ?) <~ ")") ^^ {
    case (_, Some((expression: Expression, expressionList))) => expression +: expressionList
    case _ => List.empty
  }

}
