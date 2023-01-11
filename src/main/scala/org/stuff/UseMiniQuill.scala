package org.stuff

object UseMiniQuill {
  import MiniQuill._
  import MiniQuill.Dsl._

  case class Person(id: Int, firstName: String, lastName: String)
  case class Address(fk: Int, street: String)

  def main(args: Array[String]):Unit = {

    val i = 123

    inline def q = quote(
      for {
        ii <- query[Int]
        p <- query[Person]
        a <- query[Address] if (p.id == i)
      } yield (p.firstName, a.street)
    )

    // //

    println(q)
  }
}