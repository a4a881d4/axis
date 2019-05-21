package open5g.lib.axis

import spinal.core._
import spinal.lib._

case class axis(dt:Int,ut:Int) extends Bundle {
  val user = (if(ut>0) Bits(ut bits)  else null)
  val data = Bits(dt bits)
  val last = Bool
}
