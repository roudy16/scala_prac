package fpinscala.DomainObj

import fpinscala.datastructures.PracList
import fpinscala.datastructures.Nil
import fpinscala.datastructures.Cons

case class Employee(name: String, department: String)

object EmployeeDB {
  val peeps = PracList(Employee("Joe", "Money"), Employee("Barra", "Sales"))

  def lookupByName(name: String): Option[Employee] = {
    PracList.filter(peeps)(e => e.name == name) match {
      case Nil => None
      case Cons(x, _) => Some(x)
    }
  }

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)

  def main(args: Array[String]): Unit = {
    val res = lookupByName("Joe")
    println(res)
    val reso = lookupByName("Clancy")
    println(reso)

    println(joeDepartment)
  }

}
