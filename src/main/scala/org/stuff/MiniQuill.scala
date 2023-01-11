package org.stuff

import scala.quoted._

object MiniQuill:
  import Ast._

  object Dsl {

    def query[T]: Query[T] = failNotQuoted("query[T]")

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

      /** =========================== Parse ======================== **/
      object Parser:
        import Extractors._
        def astParse(expr: Expr[Any]): Ast =
          expr match
            case '{ ($q: Quoted[t]).unquote } => astParse(q)

            case '{ Quoted.apply[t]($astExpr, $livenessExpr) } =>
              Unlifter.liveness(livenessExpr) match
                case Liveness.Static =>
                  Unlifter(astExpr)
                case Liveness.Dynamic =>
                  report.warning("Using a quotation that has Dynamic Liveness. Whole AST be wrapped into a vase.")
                  Vase.RuntimeExpression(astExpr)

            case '{ query[t] } => Entity(TypeRepr.of[t].classSymbol.get.name)
            case '{ ($query: Query[t]).filter(${Lambda1(alias, body)}) } => Filter(astParse(query), Ident(alias), astParse(body))
            case '{ ($query: Query[t]).withFilter(${Lambda1(alias, body)}) } => Filter(astParse(query), Ident(alias), astParse(body))
            case '{ ($query: Query[t]).map[mt](${Lambda1(alias, body)}) } => Map(astParse(query), Ident(alias), astParse(body))
            case '{ ($query: Query[t]).flatMap[mt](${Lambda1( alias, body)}) } => FlatMap(astParse(query), Ident(alias), astParse(body))

            case NamedOp1(left, "==", right) => BinaryOperation(astParse(left), Operator.==, astParse(right))
            case NamedOp1(left, "&&", right) => BinaryOperation(astParse(left), Operator.&&, astParse(right))
            case NamedOp1(left, "||", right) => BinaryOperation(astParse(left), Operator.||, astParse(right))
            case NamedOp1(left, "+", right) => BinaryOperation(astParse(left), Operator.+, astParse(right))
            case NamedOp1(left, "-", right) => BinaryOperation(astParse(left), Operator.-, astParse(right))
            case NamedOp1(left, "*", right) => BinaryOperation(astParse(left), Operator.*, astParse(right))
            case NamedOp1(left, "/", right) => BinaryOperation(astParse(left), Operator./, astParse(right))

            case Unseal(Apply(TypeApply(Select(TupleIdent(), "apply"), types), values)) => Tuple(values.map(v => astParse(v.asExpr)))
            case block @ Unseal(TBlock(parts, lastPart)) if (parts.length > 0) =>
              val partsAsts =
                parts.map {
                  case term: Term => astParse(term.asExpr)
                  case ValDefTerm(ast, bodyExpr) => Val(Ident(ast), astParse(bodyExpr))
                  case other => report.throwError(s"Illegal statement ${other.show} in block ${block.show}")
                }
              val lastPartAst = astParse(lastPart.asExpr)
              Block((partsAsts :+ lastPartAst))
            case Unseal(Select(TIdent(id: String), prop)) => Property(Ident(id), prop)

            case id @ Unseal(i @ TIdent(x)) =>
              val owner = Symbol.spliceOwner
              if (isTermExternal(i))
                identType(i) match
                  case IdentType.Raw =>
                    println(s"The term: ${i.show} is external to the quote")
                    Vase.RuntimeExpression('{ Constant(${i.asExpr}) })
                  case IdentType.Quoted =>
                    println(s"The quotation: ${i.show} is external to the quote")
                    val quotedExpr = i.asExprOf[Quoted[_]]
                    Vase.RuntimeExpression('{ ${quotedExpr}.ast })
                  case IdentType.Dynamic =>
                    report.throwError(s"Cannot vase a type of: ${Printer.TypeReprShortCode.show(i.tpe.widen)}. Must be a constant type or quotation of a constant type.")
              else
                Ident(x)

            case Unseal(Literal(IntConstant(i))) => Constant(i)
            case Unseal(Literal(LongConstant(i))) => Constant(i)
            case Unseal(Literal(ShortConstant(i))) => Constant(i)
            case Unseal(Literal(DoubleConstant(i))) => Constant(i)
            case Unseal(Literal(FloatConstant(i))) => Constant(i)
            case Unseal(Literal(BooleanConstant(i))) => Constant(i)
            //case Unseal(Literal(ByteConstant(i))) => Constant(i)
            case Unseal(Literal(StringConstant(i))) => Constant(i)

            // TODO need more sophisticated way of telling an external function e.g. foo.bar which would be Select(foo, "bar") etc...
            case Unseal(ApplyMatroshkaTerm(id @ ExternalIdent(name))) =>
              report.errorAndAbort(
                """|Detected a function applied that was defined outside of quote { ... }.
                   |In order to use it, apply it outside of quote { ... }, assign it to a variable,
                   |and then use the variable inside of the quotation.
                """.stripMargin,
                id.pos
              )

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
      val liveness =
        if (Ast.containsVase(quillAst))
          report.warning(s"Vase detected in the AST. Liveness will be dynamic. (${quillAst})")
          Liveness.Dynamic
        else
          report.info(s"Ast is Static: (${quillAst})")
          Liveness.Static

      val liftedQuillAst: Expr[Ast] = Lifter(quillAst)
      val livenessAst: Expr[Liveness] = Lifter.liveness(liveness)
      '{ Quoted($liftedQuillAst, $livenessAst) }
    }
  }


  /** =========================== Unlift ======================== **/
  object Unlifter:
    def apply(ast: Expr[Ast]): Quotes ?=> Ast = unliftAst.apply(ast) // can also do ast.lift but this makes some error messages simpler
    def liveness(value: Expr[Liveness]): Quotes ?=> Liveness = unliftLiveness.apply(value)

    extension [T](t: Expr[T])(using FromExpr[T], Quotes)
      def unexpr: T = t.valueOrError

    trait NiceUnliftable[T] extends FromExpr[T]:
      def unlift: Quotes ?=> PartialFunction[Expr[T], T]
      def apply(expr: Expr[T])(using Quotes): T =
        import quotes.reflect._
        unlift.lift(expr).getOrElse { throw new IllegalArgumentException(s"Could not Unlift ${Printer.TreeShortCode.show(expr.asTerm)}") }
      def unapply(expr: Expr[T])(using Quotes): Option[T] = unlift.lift(expr)

    given unliftLiveness: NiceUnliftable[Liveness] with
      def unlift =
        case '{ Liveness.Static } => Liveness.Static
        case '{ Liveness.Dynamic } => Liveness.Dynamic

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
        case '{ Operator.|| } =>  Operator.||
        case '{ Operator.+ } =>  Operator.+
        case '{ Operator.- } =>  Operator.-
        case '{ Operator.* } =>  Operator.*
        case '{ Operator./ } =>  Operator./
  end Unlifter

  /** =========================== Lift ======================== **/
  object Lifter:
    def apply(ast: Ast): Quotes ?=> Expr[Ast] = liftableAst(ast)
    def liveness(value: Liveness): Quotes ?=> Expr[Liveness] = liftableLiveness(value)

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

    given liftableLiveness : NiceLiftable[Liveness] with
      def lift =
        case Liveness.Static => '{ Liveness.Static }
        case Liveness.Dynamic => '{ Liveness.Dynamic }

    given liftableEntity : NiceLiftable[Entity] with
      def lift =
        case Entity(name: String) => '{ Entity(${name.expr})  }

    given liftableTuple: NiceLiftable[Tuple] with
      def lift =
        case Tuple(values) => '{ Tuple(${values.expr}) }

    given liftableVase: NiceLiftable[Vase] with
      def lift =
        case Vase.RuntimeExpression(content) => '{ Vase.RuntimeBinding($content) }
        // technically should not run into this case because a sheathed vase should not ever be lifted
        case Vase.RuntimeBinding(content) => '{ Vase.RuntimeBinding(${content.expr}) }

    given liftableAst : NiceLiftable[Ast] with
      def lift =
        case Constant(v: Double) => '{ Constant(${Expr(v)}) }
        case Constant(v: Boolean) => '{ Constant(${Expr(v)}) }
        case Constant(v: String) => '{ Constant(${Expr(v)}) }
        case Constant(v: Int) => '{ Constant(${Expr(v)}) }
        case Function(params: List[Ident], body: Ast) => '{ Function(${params.expr}, ${body.expr}) }
        case FunctionApply(function: Ast, values: List[Ast]) => '{ FunctionApply(${function.expr}, ${values.expr}) }
        case v: Entity => liftableEntity(v)
        case v: Tuple => liftableTuple(v)
        case Map(query: Ast, alias: Ident, body: Ast) => '{ Map(${query.expr}, ${alias.expr}, ${body.expr})  }
        case FlatMap(query: Ast, alias: Ident, body: Ast) => '{ FlatMap(${query.expr}, ${alias.expr}, ${body.expr})  }
        case Filter(query: Ast, alias: Ident, body: Ast) => '{ Filter(${query.expr}, ${alias.expr}, ${body.expr})  }
        case BinaryOperation(a: Ast, operator: Operator, b: Ast) =>
          val castOp = liftOperator(operator).asInstanceOf[Expr[Operator]]
          '{ BinaryOperation(${a.expr}, ${castOp}, ${b.expr})  }
        case v: Property => liftableProperty(v)
        case v: Ident => liftableIdent(v)
        case v: Vase => liftableVase(v)

    import Operator.{ == => ee}
    given liftOperator : NiceLiftable[Operator] with
      def lift =
        case _: ee.type => '{ Operator.== }
        case Operator.&& => '{ Operator.&& }
        case Operator.|| => '{ Operator.|| }
        case Operator.+ => '{ Operator.+ }
        case Operator.- => '{ Operator.- }
        case Operator.* => '{ Operator.* }
        case Operator./ => '{ Operator./ }
  end Lifter

end MiniQuill