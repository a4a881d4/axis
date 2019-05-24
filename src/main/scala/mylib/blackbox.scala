package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import open5g.lib.usrp._
import open5g.lib.axis.{axis,axisu}

import java.io.{File, FileWriter, BufferedWriter, PrintWriter}
import scala.io.Source

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
  buf.debug.I := io.i  
}

class frame extends Component {
  val io = new Bundle {
    val mac_src = in Bits(48 bits)
    val ip_src = in Bits(32 bits)
    val IN = slave Stream(axis(64))
    val clk = in Bool
    val clear = in Bool
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val OUT = master Stream(axisu(64,4))
    val debug = out Bits(32 bits)
    val udp_src = in Bits(16 bits)
    val reset = in Bool
  }
  val f = n3xx_chdr_eth_framer()
  f.io <> io
}
object BlackBoxVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new bufg)
    SpinalVerilog(new frame)
  }
}


object BlackBoxGen extends verilogParser {
  def main(args:Array[String]) {
    val fn = args(0)
    if( new File(fn).exists ) {
      val buffer = Source.fromFile(fn)
      val moduleString = buffer.getLines
      val moduleWithOutComent = removeComment(moduleString).reduce(_+"\n"+_)
      val f = getModule(moduleWithOutComent)
      val r = parserAll(parserModule,f)
      println(r)
    }
  }
} 

object usrpGen extends verilogParser {

  def subDir(dir:File):Iterator[File] ={
    val children = dir.listFiles().filter(_.isFile())
    children.toIterator 
  }
  def main(args:Array[String]) {
    println("-------------------")
    val dir = args(0)
    val d = new File(dir)
    println(d.getAbsolutePath)
    val writer = new PrintWriter(new File("usrp.v"))
    for(fn <- subDir(d)) {
      println(fn.getAbsolutePath)
      val buffer = Source.fromFile(fn.getAbsolutePath)
      val moduleString = buffer.getLines
      val moduleWithOutComent = removeComment(moduleString).reduce(_+"\n"+_)
      val f = getModule(moduleWithOutComent)
      val r = parserAll(parserModule,f)
      writer.println(r.toString)
    }
    writer.close()
  }
}