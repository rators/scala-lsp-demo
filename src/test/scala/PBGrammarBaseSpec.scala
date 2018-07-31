import cats.Eval
import org.scalatest.{Assertion, MustMatchers, WordSpec}
import parseback._
import parseback.compat.cats._

class PBGrammarBaseSpec extends PBTestBase with GrammarBase {
  "variable declaration parser" should {

    "parse a variable declaration of type klass" in {
      varDeclaration mustParse LineStream[Eval]("Builder x;")
    }

    "parse a variable declaration of a basic data type" in {
      varDeclaration mustParse LineStream[Eval]("int x;")
    }

  }
}

sealed trait PBTestBase extends WordSpec with MustMatchers {
  implicit class MustParseOps(parser: Parser[_]) {
    def mustParse[A](input: LineStream[Eval]): Assertion =
      parser(input).value.isRight mustBe true
  }
}
