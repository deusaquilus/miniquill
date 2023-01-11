package org.stuff

import org.stuff.Ast.Quoted


object UseMiniQuill {
  import MiniQuill._
  import MiniQuill.Dsl._

  case class Person(id: Int, firstName: String, lastName: String)
  case class Address(fk: Int, street: String)

  //   /*
  //   Quoted(
  //     Map(
  //       Entity(Int),
  //       Ident(v),
  //       BinaryOperation(
  //         Ident(v),+,
  //         RuntimeBinding(
  //           BinaryOperation(
  //             RuntimeBinding(Constant(123)),
  //             +,
  //             Constant(456)
  //           )
  //         )
  //       )
  //     ),
  //     Dynamic
  //   )
  //   */
  //   inline def q = quote(
  //       query[Int].map(v => v + ii)
  //   )

  //   val out = quote(q)

  //   println(q)
  // }


  def complexExample(): Unit = {
    val values = List(1,2,3,4)
    val quotedAdded = values.map(v => quote(v + 123))
    val quoteReduced = quotedAdded.reduce((q1, q2) => quote ( q1.unquote + q2.unquote ))
    val q = quote(
        query[Int].map(v => v + quoteReduced.unquote)
    )
    println(q) // //
  }








  // def outsideFunctionExample(): Unit = {
  //   val quoteOne = quote(1)
  //   val quoteTwo = quote(2)
  //   def fun(a: Int, b: Int) = a + b
  //   val q = quote {
  //     fun(quoteOne.unquote, quoteTwo.unquote)
  //   }
  //   println(q)
  // }

  // def outsideFunctionExampleTry2(): Unit = {
  //   val quoteOne = quote(1)
  //   val quoteTwo = quote(2)
  //   def fun(a: Int, b: Int) = a + b
  //   val sum = fun(quoteOne, quoteTwo)
  //   val q = quote {
  //     sum
  //   }
  //   println(q)
  // }

  // def outsideFunctionExampleTry3(): Unit = {
  //   val quoteOne = quote(1)
  //   val quoteTwo = quote(2)
  //   def fun(a: Quoted[Int], b: Quoted[Int]) = quote { a + b }
  //   val sum = fun(quoteOne, quoteTwo)
  //   val q = quote {
  //     sum
  //   }
  //   println(q)
  // }

  def main(args: Array[String]):Unit = complexExample()
}