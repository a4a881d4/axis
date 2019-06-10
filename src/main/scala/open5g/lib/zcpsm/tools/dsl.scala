package open5g.lib.zcpsm.tools


object If {
  def apply(cond: bool)(block: =>Unit) = {
    cond match {
      case x:boolC   => if(cond.Constant == true) {block} else {println("ne");block}
      case x:boolEqu => println("depend" + cond.toString)
    }
  }
}

trait baseType {
  val Constant:Any
  val isConstant:Boolean
}

trait bool extends baseType
case class boolC(Constant:Boolean) extends bool {
  val isConstant = true
}
case class boolEqu(a:number,b:number) extends bool {
  val isConstant = false
  val Constant = 0
  override def toString = "(" + a.toString + " == " + b.toString + ")" 
}
trait number extends baseType {
  def ==(that:number) : bool = {
    if(this.isConstant && that.isConstant) {
      boolC(this.Constant == that.Constant)
    } else {
      boolEqu(this,that)
    }
  }
}

case class numberC(val Constant:Int,val size:Int=1) extends number {
  val isConstant = true
}

class byte(val size:Int) extends number {
  val isConstant = false
  val Constant = 0
}

object dsltest {
  def t1 = {
    val a = numberC(3)
    val b = numberC(4)
    If(a == b) {
      println(a.toString + " == " + b.toString)
    }
    val c = new byte(2)
    If(a == c) {
      println(a.toString + " == " + c.toString)
    }
    val d = numberC(3)
    If(a == d) {
      println(a.toString + " == " + d.toString)
    }
  }
}