package org.stuff

import scala.quoted._

sealed trait Ast

object Ast {
  case class Entity(name:String) extends Ast
  case class Ident(name:String) extends Ast
  case class Filter(query:Ast, alias:Ident, body: Ast) extends Ast
  case class Map(query:Ast, alias:Ident, body: Ast) extends Ast
  case class FlatMap(query:Ast, alias:Ident, body: Ast) extends Ast
  case class Property(ast: Ast, name: String) extends Ast
  case class Tuple(values: List[Ast]) extends Ast
  case class Constant(value: Any) extends Ast
  case class Function(params: List[Ident], body: Ast) extends Ast
  case class FunctionApply(function: Ast, values: List[Ast]) extends Ast
  case class Block(statements: List[Ast]) extends Ast
  case class Val(name: Ident, body: Ast) extends Ast
  sealed trait Vase extends Ast
  object Vase:
    case class RuntimeExpression(content: Expr[Ast]) extends Vase
    case class RuntimeBinding(content: Ast) extends Vase

  sealed trait Operator
  object Operator:
    case object `==` extends Operator
    case object `&&` extends Operator
    case object `||` extends Operator
    case object `+` extends Operator
    case object `-` extends Operator
    case object `*` extends Operator
    case object `/` extends Operator

  sealed trait Liveness
  object Liveness:
    case object Static extends Liveness
    case object Dynamic extends Liveness

  case class BinaryOperation(left:Ast, op:Operator, right:Ast) extends Ast

  case class Quoted[T](ast: Ast, liveness: Liveness):
    def unquote: T = fail("A quoted term can only be injected (i.e. unquoted) into a Scala expression that is inside of a quoted { ... } block.")

  implicit inline def autoUnquote[T](inline t: Quoted[T]): T = t.unquote

  class Query[T] {
    def filter(e:T => Boolean): Query[T] 	    = failNotQuoted("filter")
    def withFilter(e:T => Boolean): Query[T] 	    = failNotQuoted("withFilter")
    def map[R](e:T => R): Query[R]     			  = failNotQuoted("map")
    def flatMap[R](e:T => Query[R]): Query[R] = failNotQuoted("flatMap")
  }

  def containsVase(ast: Ast): Boolean =
    ast match
      case Entity(name) => false
      case Ident(name) => false
      case Filter(query, alias, body) => containsVase(query) || containsVase(body)
      case Map(query, alias, body) => containsVase(query) || containsVase(body)
      case FlatMap(query, alias, body) => containsVase(query) || containsVase(body)
      case Property(ast, name) => containsVase(ast)
      case Tuple(values) => values.exists(containsVase(_))
      case Constant(value) => false
      case Function(params, body) => containsVase(ast)
      case FunctionApply(function, values) => containsVase(function) || values.exists(containsVase(_))
      case Block(statements) => statements.exists(containsVase(_))
      case Val(name, body) => containsVase(body)
      case BinaryOperation(left, op, right) => containsVase(left) || containsVase(right)
      case Vase.RuntimeExpression(content) => true
      case Vase.RuntimeBinding(content) => true
}

def failNotQuoted(termType: String) =
  throw new IllegalArgumentException(s"A '$termType' can only be used in a quote { ... } block. Be sure to encapsulate it into a quote block before using.")

def fail(error: String) =
  throw new IllegalArgumentException(error)