import parseback._

case class VariableDeclaration(`type`: Type, id: Identifier)

case class FieldDeclaration(`type`: Type, id: Identifier)

sealed trait Symbol {
  val lines: List[Line]
}

sealed trait Expression

case object ThisExpression extends Expression

case class ParenExpression(underlying: Expression) extends Expression

case class IdentifierExpression(underlying: Identifier) extends Expression

case class ArrayAccessExpression(array: Expression, index: Expression) extends Expression

case class ArrayLengthExpression(array: Expression) extends Expression

case class MethodCallExpression(subject: Expression, methodName: Identifier, arguments: List[Expression]) extends Expression

case class UnaryOperatorExpression(op: UnaryOperator, expression: Expression) extends Expression

sealed trait UnaryOperator

case object Not extends UnaryOperator

case object Neg extends UnaryOperator

case class ArrayInstantiationExpression(length: Expression) extends Expression

case class ObjectInstantiationExpression(klassIdentifier: KlassIdentifier) extends Expression

case class BinaryOperatorExpression(left: Expression, op: BinaryOperator, right: Expression) extends Expression

sealed trait BinaryOperator

case object Plus extends BinaryOperator

case object Subtract extends BinaryOperator

case object Multiply extends BinaryOperator

case object LessThan extends BinaryOperator

case object And extends BinaryOperator

sealed trait Literal extends Expression

case class IntLiteral(int: Int) extends Literal

case class BooleanLiteral(bool: Boolean) extends Literal

sealed trait Type

sealed trait NamedSymbol {
  val name: String
}

case class FormalParameter(`type`: Type, id: Identifier)

case object Int extends Type

case object Bool extends Type

case object IntArr extends Type

case class KlassIdentifier(name: String) extends Type with NamedSymbol

case class Identifier(name: String) extends NamedSymbol

trait GrammarBase {

  implicit val WS: Whitespace = Whitespace(() | """[\s\r\t\n]+""".r)

  lazy val varDeclaration: Parser[VariableDeclaration] = (`type` ~ ID <~ ";") ^^ { (_, t, id) => VariableDeclaration(t, id) }
  lazy val fieldDeclaration: Parser[FieldDeclaration] = (`type` ~ ID <~ ";") ^^ { (_, t, id) => FieldDeclaration(t, id) }

  lazy val formalParameters: Parser[Option[List[FormalParameter]]] = "(" ~> (formalParameterList ?) <~ ")"
  lazy val formalParameterList: Parser[List[FormalParameter]] = (formalParameter ~ ("," ~ formalParameter *)) ^^ {
    case (_, formalParameter: FormalParameter, formalParamParseList) =>
      formalParameter +: formalParamParseList.map(_._2)
  }
  lazy val formalParameter: Parser[FormalParameter] = (`type` ~ ID) ^^ { (_, t, i) => FormalParameter(t, i) }
  lazy val `type`: Parser[Type] = intArrayType | booleanType | intType | klassType

  lazy val intArrayType: Parser[IntArr.type] = ("int" ~ "[" ~ "]") ^^^ IntArr

  lazy val booleanType: Parser[Bool.type] = "boolean" ^^^ Bool

  lazy val intType: Parser[Int.type] = "int" ^^^ Int

  lazy val klassType: Parser[KlassIdentifier] = ID.filter(!_.name.matches("int|boolean|true|false")).map(id => KlassIdentifier(id.name))

  lazy val ID: Parser[Identifier] = """[a-zA-Z_][0-9a-zA-Z_]*""".r ^^ { (_, id) => Identifier(id) }

  lazy val BOOL: Parser[Boolean] = ("true" | "false") ^^ { (_, bool) => bool.toBoolean }
  lazy val INT: Parser[Int] = """\d+""".r ^^ { (_, num) => num.toInt }
}

trait ExpressionGrammar extends GrammarBase {
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

  lazy val notExpression: Parser[UnaryOperatorExpression] = ("!" ~> expression) ^^ { (_, e) => UnaryOperatorExpression(Not, e) }
  lazy val negExpression: Parser[UnaryOperatorExpression] = ("-" ~> expression) ^^ { (_, e) => UnaryOperatorExpression(Neg, e) }
  lazy val arrayInstantiationExpression: Parser[ArrayInstantiationExpression] = (("new" ~> "int") ~> ("[" ~> expression <~ "]")) ^^ {
    (_, e) => ArrayInstantiationExpression(e)
  }
  lazy val objectInstantiationExpression: Parser[ObjectInstantiationExpression] = ("new" ~> klassType <~ ("(" ~ ")")) ^^ {
    (_, e) => ObjectInstantiationExpression(e)
  }
  lazy val binaryOperatorExpression: Parser[BinaryOperatorExpression] = (expression ~ binaryOperator ~ expression) ^^ { (_, lh, op, rh) => BinaryOperatorExpression(lh, op, rh) }

  lazy val identifierExpression: Parser[IdentifierExpression] = ID.map(IdentifierExpression)
  lazy val thisExpression: Parser[ThisExpression.type] = "this" ^^^ ThisExpression
  lazy val parenExpression: Parser[ParenExpression] = ("(" ~> expression <~ ")") ^^ { (_, e) => ParenExpression(e) }

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