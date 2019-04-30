package mylib

import spinal.core._
import spinal.lib._

import open5g.lib.axis._

class axisdut extends Component {
  val config = AxisConfig(64)
  val io = new Bundle {
    val xi = slave(Axis(config,4))
    val xo = master(Axis(config,4))
  }
  io.xi >> io.xo
}

//Generate the MyTopLevel's Verilog
object MyDutVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new axisdut)
  }
}

