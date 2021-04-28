package org.stuff

import scala.quoted._

object MiniQuill:

  sealed trait Ast
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

  sealed trait Operator
  object Operator:
    case object `==` extends Operator
    case object `&&` extends Operator

  case class BinaryOperation(left:Ast, op:Operator, right:Ast) extends Ast

  case class Quoted[T](ast: Ast):
    def unquote = throw new IllegalArgumentException("Only a compile-time-construct")

  class Query[T] {
    def filter(e:T => Boolean): Query[T] 	    = throw new IllegalArgumentException("This can only be used inside a quoted block")
    def withFilter(e:T => Boolean): Query[T] 	    = throw new IllegalArgumentException("This can only be used inside a quoted block")
    def map[R](e:T => R): Query[R]     			  = throw new IllegalArgumentException("This can only be used inside a quoted block")
    def flatMap[R](e:T => Query[R]): Query[R] = throw new IllegalArgumentException("This can only be used inside a quoted block")
  }

  object Dsl {
    def query[T]: Query[T] = throw new IllegalArgumentException("This can only be used inside a quoted block")

    inline def unquote[T](inline quoted:Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
    def unquoteImpl[T:Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = {
      import quotes.reflect._
      '{ $quoted.unquote } /*Quoted[Query[T]] => Query[T]*/
    }
    implicit inline def autoUnquote[T](inline quoted: Quoted[T]): T = unquote(quoted)

    /** ============================================ Quotation ===================================== **/
    inline def quote[T](inline quoted:T): Quoted[T] = ${ quoteImpl[T]('quoted) }
    def quoteImpl[T:Type](quoted: Expr[T])(using Quotes): Expr[Quoted[T]] = {
      import quotes.reflect.{Ident => TIdent, Constant => TConstant, Block => TBlock, ValDef => TValDef, _}

      val quotedRaw = quoted.asTerm.underlyingArgument.asExpr

      /** =========================== Parsing Helpers ======================== **/
      object Extractors:
        object Lambda1:
          def unapplyTerm(term: Term): Option[(String, Term)] = 
            term match
              case Lambda(List(TValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
              case TBlock(List(), expr) => unapplyTerm(expr)
              case _ => None

          def unapply(term: Expr[_]): Option[(String, Expr[_])] =
            unapplyTerm(term.asTerm).map((str, term) => (str, term.asExpr))
        end Lambda1

        object UntypeExpr:
          def unapply(expr: Expr[_]): Option[Expr[_]] = Untype.unapply(expr.asTerm).map(_.asExpr)
          def apply(expr: Expr[_]): Expr[_] = Untype.unapply(expr.asTerm).map(_.asExpr).get

        object Untype:
          def unapply(term: Term): Option[Term] = term match
            case TypedMatroshkaTerm(t) => Some(t)
            case other => Some(other)
          def apply(term: Term) = Untype.unapply(term).get

        object TypedMatroshkaTerm:
          def recurse(innerTerm: Term): Term = innerTerm match
            case Typed(innerTree, _) => recurse(innerTree)
            case other => other
          def unapply(term: Term): Option[Term] = term match
            case Typed(tree, _) => Some(recurse(tree))
            case other => None

        object Unseal:
          def unapply(expr: Expr[_]): Option[Term] = Some(expr.asTerm)

        object TupleName:
          def unapply(str: String): Boolean = str.matches("Tuple[0-9]+")

        object TupleIdent:
          def unapply(term: Term): Boolean =
            term match
              case TIdent(TupleName()) => true
              case _ => false

        object NamedOp1:
          def unapply(expr: Expr[_]): Option[(Expr[_], String, Expr[_])] =
            UntypeExpr(expr) match
              case Unseal(Apply(Select(Untype(left), op: String), Untype(right) :: Nil)) => Some(left.asExpr, op, right.asExpr)
              case _ => None

        object ValDefTerm:
          def unapply(tree: Tree): Option[(Ident, Expr[_])] =
            tree match {
              case DefDef(name, paramss, tpe, rhsOpt) if (paramss.length == 0) =>
                val body =
                  rhsOpt match
                    case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
                    case Some(rhs) => rhs
                Some(Ident(name), body.asExpr)
              case TValDef(name, tpe, rhsOpt) =>
                val body =
                  rhsOpt match
                    case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
                    case Some(rhs) => rhs
                Some((Ident(name), body.asExpr))
              case _ => None
            }
        end ValDefTerm
      end Extractors

      /** =========================== Parse ======================== **/
      object Parser:
        import Extractors._
        def astParse(expr: Expr[Any]): Ast = 
          expr match
            case '{ ($q: Quoted[t]).unquote } => astParse(q)
            case '{ Quoted.apply[t]($ast) } => Unlifter(ast)
            case '{ query[t] } => Entity(TypeRepr.of[t].classSymbol.get.name)
            case '{ ($query: Query[t]).filter(${Lambda1(alias, body)}) } => Filter(astParse(query), Ident(alias), astParse(body))
            case '{ ($query: Query[t]).withFilter(${Lambda1(alias, body)}) } => Filter(astParse(query), Ident(alias), astParse(body))
            case '{ ($query: Query[t]).map[mt](${Lambda1(alias, body)}) } => Map(astParse(query), Ident(alias), astParse(body))
            case '{ ($query: Query[t]).flatMap[mt](${Lambda1(alias, body)}) } => FlatMap(astParse(query), Ident(alias), astParse(body))
            case NamedOp1(left, "==", right) => BinaryOperation(astParse(left), Operator.==, astParse(right))
            case NamedOp1(left, "&&", right) => BinaryOperation(astParse(left), Operator.&&, astParse(right))
            case Unseal(Apply(TypeApply(Select(TupleIdent(), "apply"), types), values)) => Tuple(values.map(v => astParse(v.asExpr)))
            case block @ Unseal(TBlock(parts, lastPart)) if (parts.length > 0) =>
              val partsAsts =
                parts.map {
                  case term: Term => astParse(term.asExpr)
                  case ValDefTerm(ast, bodyExpr) => Val(ast, astParse(bodyExpr))
                  case other => report.throwError(s"Illegal statement ${other.show} in block ${block.show}")
                }
              val lastPartAst = astParse(lastPart.asExpr)
              Block((partsAsts :+ lastPartAst))
            case Unseal(Select(TIdent(id: String), prop)) => Property(Ident(id), prop)
            case id @ Unseal(i @ TIdent(x)) => Ident(x)
            case Unseal(Typed(inside /*Term*/, _)) => astParse(inside.asExpr)
            case _ => report.throwError(
              s"""
              |Cannot parse the tree: 
              |=================== Simple ==============
              |${Printer.TreeShortCode.show(expr.asTerm)}
              |=================== Full AST ==============
              |${Printer.TreeStructure.show(expr.asTerm)}
              """.stripMargin)
      end Parser

      val quillAst: Ast = Parser.astParse(quotedRaw)
      val liftedQuillAst: Expr[Ast] = Lifter(quillAst)
      '{ Quoted($liftedQuillAst) }
    }
  }


  /** =========================== Unlift ======================== **/
  object Unlifter:
    def apply(ast: Expr[Ast]): Quotes ?=> Ast = unliftAst.apply(ast) // can also do ast.lift but this makes some error messages simpler

    extension [T](t: Expr[T])(using FromExpr[T], Quotes)
      def unexpr: T = t.valueOrError

    trait NiceUnliftable[T] extends FromExpr[T]:
      def unlift: Quotes ?=> PartialFunction[Expr[T], T]
      def apply(expr: Expr[T])(using Quotes): T = 
        import quotes.reflect._
        unlift.lift(expr).getOrElse { throw new IllegalArgumentException(s"Could not Unlift ${Printer.TreeShortCode.show(expr.asTerm)}") }
      def unapply(expr: Expr[T])(using Quotes): Option[T] = unlift.lift(expr)

    given unliftProperty: NiceUnliftable[Property] with
      def unlift =
        case '{ Property(${ast}, ${Expr(name: String)}) } => Property(ast.unexpr, name)

    given unliftIdent: NiceUnliftable[Ident] with
      def unlift =
        case '{ Ident(${Expr(name: String)}) } => Ident(name)

    given unliftAst: NiceUnliftable[Ast] with
      def unlift =
        case '{ Constant(${Expr(b: Double)}: Double) } => Constant(b)
        case '{ Constant(${Expr(b: Boolean)}: Boolean) } => Constant(b)
        case '{ Constant(${Expr(b: String)}: String) } => Constant(b)
        case '{ Constant(${Expr(b: Int)}: Int) } => Constant(b)
        case '{ Entity.apply(${Expr(b: String)})  } => Entity(b)
        case '{ Function($params, $body) } => Function(params.unexpr, body.unexpr)
        case '{ FunctionApply($function, $values) } => FunctionApply(function.unexpr, values.unexpr)
        case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(query.unexpr, alias.unexpr, body.unexpr)
        case '{ FlatMap(${query}, ${alias}, ${body}: Ast) } => FlatMap(query.unexpr, alias.unexpr, body.unexpr)
        case '{ Filter(${query}, ${alias}, ${body}: Ast) } => Filter(query.unexpr, alias.unexpr, body.unexpr)
        case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(a.unexpr, unliftOperator(operator).asInstanceOf[Operator], b.unexpr)
        case '{ Property(${ast}, ${Expr(name: String)}) } => Property(ast.unexpr, name)
        case '{ Tuple.apply($values) } => Tuple(values.unexpr)
        case '{ $p: Property } => unliftProperty(p)
        case '{ $id: Ident } => unliftIdent(id)

    given unliftOperator: NiceUnliftable[Operator] with
      def unlift = 
        case '{ Operator.== } =>  Operator.==
        case '{ Operator.&& } =>  Operator.&&
  end Unlifter

  /** =========================== Lift ======================== **/
  object Lifter:
    def apply(ast: Ast): Quotes ?=> Expr[Ast] = liftableAst(ast)

    extension [T](t: T)(using ToExpr[T], Quotes)
      def expr: Expr[T] = Expr(t)

    trait NiceLiftable[T] extends ToExpr[T]:
      def lift: Quotes ?=> PartialFunction[T, Expr[T]]
      def apply(t: T)(using Quotes): Expr[T] = lift.lift(t).getOrElse { throw new IllegalArgumentException(s"Could not Lift ${t}") }
      def unapply(t: T)(using Quotes) = Some(apply(t))

    given liftableProperty : NiceLiftable[Property] with
      def lift =
        case Property(core: Ast, name: String) => '{ Property(${core.expr}, ${name.expr}) }

    given liftableIdent : NiceLiftable[Ident] with
      def lift =
        case Ident(name: String) => '{ Ident(${name.expr})  }

    extension [T: Type](list: List[T])(using ToExpr[T], Quotes)
      def spliceVarargs = Varargs(list.map(Expr(_)).toSeq)

    given liftableEntity : NiceLiftable[Entity] with
      def lift = 
        case Entity(name: String) => '{ Entity(${name.expr})  }

    given liftableTuple: NiceLiftable[Tuple] with
      def lift = 
        case Tuple(values) => '{ Tuple(${values.expr}) }

    given liftableAst : NiceLiftable[Ast] with
      def lift =
        case Constant(v: Double, quat) => '{ Constant(${Expr(v)}) }
        case Constant(v: Boolean, quat) => '{ Constant(${Expr(v)}) }
        case Constant(v: String, quat) => '{ Constant(${Expr(v)}) }
        case Constant(v: Int, quat) => '{ Constant(${Expr(v)}) }
        case Function(params: List[Ident], body: Ast) => '{ Function(${params.expr}, ${body.expr}) }
        case FunctionApply(function: Ast, values: List[Ast]) => '{ FunctionApply(${function.expr}, ${values.expr}) }
        case v: Entity => liftableEntity(v)
        case v: Tuple => liftableTuple(v)
        case Map(query: Ast, alias: Ident, body: Ast) => '{ Map(${query.expr}, ${alias.expr}, ${body.expr})  }
        case FlatMap(query: Ast, alias: Ident, body: Ast) => '{ FlatMap(${query.expr}, ${alias.expr}, ${body.expr})  }
        case Filter(query: Ast, alias: Ident, body: Ast) => '{ Filter(${query.expr}, ${alias.expr}, ${body.expr})  }
        case BinaryOperation(a: Ast, operator: Operator, b: Ast) => '{ BinaryOperation(${a.expr}, ${liftOperator(operator).asInstanceOf[Expr[Operator]]}, ${b.expr})  }
        case v: Property => liftableProperty(v)
        case v: Ident => liftableIdent(v)

    import Operator.{ == => ee}
    given liftOperator : NiceLiftable[Operator] with
      def lift =
        case _: ee.type => '{ Operator.== }
        case Operator.&& => '{ Operator.&& }
  end Lifter

end MiniQuill