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
class fifodut extends Component {
  val acfg = AxisConfig(64)
  val fcfg = AxisFifoConfig(acfg,4,9)
  val dut = AxisFifoRam(fcfg)
  val io = new Bundle {
    val d = slave(Axis(acfg,4))
    val q = master(Axis(acfg,4))
    val c = in Bool
    val s = out UInt(10 bits)
    val o = out UInt(10 bits)
  } 
  io.d >> dut.io.d
  dut.io.q >> io.q
  dut.io.clear := io.c
  io.s := dut.state.space
  io.o := dut.state.occupied 
}

object MyFifoSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new fifodut){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var idx = 0

      dut.io.c #= true
      dut.clockDomain.waitRisingEdge()

      while(idx<1024) {
        dut.io.c #= false
        dut.io.d.tdata #= 0
        
      }
//Generate the MyTopLevel's Verilog
object MyDutVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new axisdut)
  }
}

