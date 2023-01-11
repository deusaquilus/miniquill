package org.stuff

import scala.quoted.*

/** =========================== Parsing Helpers ======================== **/
object Extractors:

  def isTermExternal(using Quotes)(term: quotes.reflect.Term): Boolean =
    import quotes.reflect.*
    println(s"====== Owners: ${ownershipChain(term)} == Contain ${Symbol.spliceOwner} ? === ${ownershipChain(term).contains(Symbol.spliceOwner)}")
    !ownershipChain(term).contains(Symbol.spliceOwner)
  end isTermExternal

  object ExternalIdent:
    def unapply(using Quotes)(term: quotes.reflect.Tree): Option[String] =
      import quotes.reflect.*
      term match
        case id: Ident if (isTermExternal(id)) => Some(id.name)
        case _ => None

  enum IdentType:
    case Raw
    case Quoted
    case Dynamic

  def identType(using Quotes)(i: quotes.reflect.Ident) =
    import quotes.reflect._
    import Ast.Quoted
    i.tpe.asType match
      case '[Int] => IdentType.Raw
      case '[Long] => IdentType.Raw
      case '[Short] => IdentType.Raw
      case '[Double] => IdentType.Raw
      case '[Float] => IdentType.Raw
      case '[Boolean] => IdentType.Raw
      case '[Byte] => IdentType.Raw
      case '[String] => IdentType.Raw
      case '[Quoted[Int]] => IdentType.Quoted
      case '[Quoted[Long]] => IdentType.Quoted
      case '[Quoted[Short]] => IdentType.Quoted
      case '[Quoted[Double]] => IdentType.Quoted
      case '[Quoted[Float]] => IdentType.Quoted
      case '[Quoted[Boolean]] => IdentType.Quoted
      case '[Quoted[Byte]] => IdentType.Quoted
      case '[Quoted[String]] => IdentType.Quoted
      case _ => IdentType.Dynamic

  def ownershipChain(using Quotes)(term: quotes.reflect.Term): List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    def ownershipChainRecurse(sym: Symbol, accum: List[Symbol]): List[Symbol] =
      if (sym.isNoSymbol)
        accum.reverse
      else
        ownershipChainRecurse(sym.owner, sym +: accum)
    end ownershipChainRecurse
    ownershipChainRecurse(term.symbol, List())
  end ownershipChain

  object Lambda1:
    def unapplyTerm(using Quotes)(term: quotes.reflect.Term): Option[(String, quotes.reflect.Term)] =
      import quotes.reflect.{ValDef => TValDef, Block => TBlock, *}
      term match
        case Lambda(List(TValDef(ident, Inferred(), None)), methodBody) => Some((ident, methodBody))
        case TBlock(List(), expr) => unapplyTerm(expr)
        case _ => None

    def unapply(term: Expr[_])(using Quotes): Option[(String, Expr[_])] =
      import quotes.reflect.*
      unapplyTerm(term.asTerm).map((str, term) => (str, term.asExpr))
  end Lambda1

  object UntypeExpr:
    def unapply(expr: Expr[_])(using Quotes): Option[Expr[_]] =
      import quotes.reflect.*
      Untype.unapply(expr.asTerm).map(_.asExpr)

    def apply(expr: Expr[_])(using Quotes): Expr[_] =
      import quotes.reflect.*
      Untype.unapply(expr.asTerm).map(_.asExpr).get

  object Untype:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] = term match
      case TypedMatroshkaTerm(t) => Some(t)
      case other => Some(other)

    def apply(using Quotes)(term: quotes.reflect.Term) = Untype.unapply(term).get

  object ApplyMatroshkaTerm:
    private object ApplyThing:
      def unapply(using Quotes)(term: quotes.reflect.Term) =
        import quotes.reflect.*
        term match
          case Apply(core, _) => Some(core)
          case TypeApply(core, _) => Some(core)
          case Typed(core, _) => Some(core)
          case _ => None

    def recurse(using Quotes)(innerTerm: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect.*
      innerTerm match
        case ApplyThing(innerTree) => recurse(innerTree)
        case other => other

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect.*
      term match
        case ApplyThing(tree) => Some(recurse(tree))
        case other => None
  end ApplyMatroshkaTerm

  object TypedMatroshkaTerm:
    def recurse(using Quotes)(innerTerm: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect.*
      innerTerm match
        case Typed(innerTree, _) => recurse(innerTree)
        case other => other

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect.*
      term match
        case Typed(tree, _) => Some(recurse(tree))
        case other => None
  end TypedMatroshkaTerm

  object Unseal:
    def unapply(expr: Expr[_])(using Quotes): Option[quotes.reflect.Term] =
      import quotes.reflect.*
      Some(expr.asTerm)

  object TupleName:
    def unapply(str: String): Boolean = str.matches("Tuple[0-9]+")

  object TupleIdent:
    def unapply(using Quotes)(term: quotes.reflect.Term): Boolean =
      import quotes.reflect.{Ident => TIdent, *}
      term match
        case TIdent(TupleName()) => true
        case _ => false

  object NamedOp1:
    def unapply(expr: Expr[_])(using Quotes): Option[(Expr[_], String, Expr[_])] =
      import quotes.reflect.*
      UntypeExpr(expr) match
        case Unseal(Apply(Select(Untype(left), op: String), Untype(right) :: Nil)) => Some(left.asExpr, op, right.asExpr)
        case _ => None

  object ValDefTerm:
    def unapply(using Quotes)(tree: quotes.reflect.Tree): Option[(String, Expr[_])] =
      import quotes.reflect.{ValDef => TValDef, *}
      tree match {
        case DefDef(name, paramss, tpe, rhsOpt) if (paramss.length == 0) =>
          val body =
            rhsOpt match
              case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
              case Some(rhs) => rhs
          Some(name, body.asExpr)
        case TValDef(name, tpe, rhsOpt) =>
          val body =
            rhsOpt match
              case None => report.throwError(s"Cannot parse 'val' clause with no '= rhs' (i.e. equals and right hand side) of ${Printer.TreeStructure.show(tree)}")
              case Some(rhs) => rhs
          Some((name, body.asExpr))
        case _ => None
      }
  end ValDefTerm
end Extractors