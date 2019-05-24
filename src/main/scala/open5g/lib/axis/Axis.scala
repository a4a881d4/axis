package open5g.lib.axis

import spinal.core._
import spinal.lib._

case class axisu(dt:Int,ut:Int) extends Bundle {
  val user = Bits(ut bits)
  val data = Bits(dt bits)
  val last = Bool
}

case class axis(dt:Int) extends Bundle {
  val data = Bits(dt bits)
  val last = Bool
}
