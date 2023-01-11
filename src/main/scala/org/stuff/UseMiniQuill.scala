package org.stuff

object UseMiniQuill {
  import MiniQuill._
  import MiniQuill.Dsl._

  case class Person(id: Int, firstName: String, lastName: String)
  case class Address(fk: Int, street: String)

  def main(args: Array[String]):Unit = {

    val i = 123
    inline def iInline = 123

    val iQuoted = quote(123)
    println(s"iQuoted: ${iQuoted}")

    // val q = quote(
    //   for {
    //     //ii <- query[Int]
    //     p <- query[Person]
    //     a <- query[Address] if (p.id == iQuoted.unquote)
    //   } yield (p.firstName, a.street)
    // )

    val q = quote(
        query[Int].map(v => v + iQuoted.unquote)
    )



    println(q) // //
  }
}