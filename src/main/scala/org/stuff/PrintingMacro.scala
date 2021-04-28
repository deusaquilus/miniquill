package org.stuff

import scala.quoted._

object PrintMac {
  inline def apply(inline any: Any): Unit = ${ printImpl('any) }
  def printImpl(expr: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    println("================== The Short Version ================")
    println(expr.show)
    println("================== The Long Version ================")
    println(Printer.TreeStructure.show(expr.asTerm.underlyingArgument))
    '{ () }
  }
}
