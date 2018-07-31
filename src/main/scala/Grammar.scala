import parseback._

case class VariableDeclaration(`type`: Type, id: Identifier)

case class FieldDeclaration(`type`: Type, id: Identifier)

sealed trait Symbol {
  val lines: List[Line]
}

sealed trait Expression

case class ArrayAccessExpression(array: Expression, index: Expression) extends Expression

case class ArrayLengthExpression(array: Expression) extends Expression

case class MethodCallExpression(subject: Expression, methodName: Identifier, arguments: List[Expression]) extends Expression

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

  lazy val klassType: Parser[KlassIdentifier] = ID.filter(!_.name.matches("int|boolean")).map(id => KlassIdentifier(id.name))

  lazy val ID: Parser[Identifier] = """[a-zA-Z_][0-9a-zA-Z_]*""".r ^^ { (_, id) => Identifier(id) }

  lazy val BOOL: Parser[Boolean] = ("true" | "false") ^^ { (_, bool) => bool.toBoolean }
  lazy val INT: Parser[Int] = """\d+""".r ^^ { (_, num) => num.toInt }
}

trait ExpressionGrammar extends GrammarBase {
  lazy val expression: Parser[Expression] =
    arrayAccessExpression |
      arrayLengthExpression |
      methodCallExpression

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

  lazy val methodArgumentList: Parser[List[Expression]] = ("(" ~> (expression ~ (("," ~> expression) *) ?) <~ ")") ^^ {
    case (_, Some((expression: Expression, expressionList))) => expression +: expressionList
    case _ => List.empty
  }

}