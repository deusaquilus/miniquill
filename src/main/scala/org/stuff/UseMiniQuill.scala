package org.stuff

object UseMiniQuill {
  import MiniQuill._
  import MiniQuill.Dsl._

  case class Person(id: Int, firstName: String, lastName: String)
  case class Address(fk: Int, street: String)

  def main(args: Array[String]):Unit = {
    inline def q = quote(
      for {
        p <- query[Person]
        a <- query[Address] if (p.id == a.fk)
      } yield (p.firstName, a.street)
    )

    println(q)
  }
}