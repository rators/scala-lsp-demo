package model

import cats.data.NonEmptyList

// TODO add 'Line' property to all models
case class VariableDeclaration(`type`: Type, id: Identifier)

case class FieldDeclaration(`type`: Type, id: Identifier)

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

sealed trait Statement

case class NestedStatement(statements: List[Statement]) extends Statement

case class IfElseStatement(ifExpression: Expression, thenStatement: Statement, elseStatement: Statement) extends Statement

case class WhileStatement(predicateExpression: Expression, doStatement: Statement) extends Statement

case class PrintStatement(printExpression: Expression) extends Statement

case class AssignStatement(id: Identifier, value: Expression) extends Statement

case class ArrayAssignStatement(id: Identifier, indexExpression: Expression, value: Expression) extends Statement

case class ReturnStatement(value: Expression) extends Statement

case class MethodBody(declarations: List[VariableDeclaration], nonEmptyList: NonEmptyList[Statement])
