package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import open5g.lib.usrp._

class bufg extends Component {

  val io = new Bundle {
    val i = in Bool
    val ib = in Bool
    val o = out Bool
  }

  val buf = new IBFGDS
  buf.io.I := io.i
  buf.io.IB := io.ib
  io.o := buf.io.O  
}
object BlackBoxVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new bufg)
  }
}


object BlackBoxGen extends verilogParser {
  def main(args:Array[String]) {
    val r = parserAll(parserModule,
      """
        |module dut # (parameter A = 1) (
        |input wire clk,
        |input wire rst,
        |input wire [31:0] data,
        |output wire [31:0] q
        |);
      """.stripMargin)
 
    println(r)
  }
} 
