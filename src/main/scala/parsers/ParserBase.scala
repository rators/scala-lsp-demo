package parsers

import model._
import parseback._

trait ParserBase {

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
