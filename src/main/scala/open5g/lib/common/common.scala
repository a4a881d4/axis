package open5g.lib.common

import spinal.core._
import spinal.lib._

case class ShiftReg(width:Int,deep:Int) extends Component {
  val io = new Bundle {
    val d = in Bits(width bits)
    val q = out Bits(width bits)
    val s = out Bits(width bits)
    val ce = in Bool
  }
  val mem = Reg(Vec(Bits(width bits),deep+1))
  val q = Reg(Bits(width bits)) init 0
  when(io.ce) {
    mem(0) := io.d
    for(i <- 0 until deep) {
      mem(i+1) := mem(i)
    }
    q := mem(deep-1)
  }
  io.q := q
  io.s := mem(deep)
}

case class Constant(value:Int,size:Int) {
  def toBits = B(value,size bits)
  def is(b:Bits) = b === toBits
}

case class clock() extends Bundle with IMasterSlave {
  val clk = Bool
  val rst = Bool
  def asMaster = {
    out(clk,rst)
  }
  def clkDomain = ClockDomain(clk,rst)
}