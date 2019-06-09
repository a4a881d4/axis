package mylib

trait opCode {
  val opc : Int
}

case class aCaseClass(val opc:Int = 1) extends opCode

object mytest {
  def main(args: Array[String]) {
    val a = aCaseClass()
    println(a.opc)
  }
}
