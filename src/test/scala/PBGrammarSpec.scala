import cats.Eval
import org.scalatest.{Assertion, MustMatchers, WordSpec}
import parseback._
import parseback.compat.cats._
import parsers.ExpressionParser

class PBGrammarSpec extends PBTestBase with ExpressionParser {
  "variable declaration parser" should {

    "parse a variable declaration of type klass" in {
      varDeclaration mustParse LineStream[Eval]("Builder x;")
    }

    "parse a variable declaration of a basic data type" in {
      varDeclaration mustParse LineStream[Eval]("int x;")
    }

  }

  "expression parser" should {
    "parse a object instantiation expression" in {
      expression mustParse LineStream[Eval]("new Koala()")
    }

    "parse a method call expression" in {
      expression mustParse LineStream[Eval]("new Koala().doSomething(1,2,3)")
    }
  }
}

sealed trait PBTestBase extends WordSpec with MustMatchers {
  implicit class MustParseOps(parser: Parser[_]) {
    def mustParse[A](input: LineStream[Eval]): Assertion =
      parser(input).value.isRight mustBe true
  }
}
