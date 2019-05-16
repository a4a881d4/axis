package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import open5g.lib.usrp._
import java.io.{File, FileWriter, BufferedWriter}
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
}
object BlackBoxVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new bufg)
  }
}


object BlackBoxGen extends verilogParser {
  def remove(x:String) = {
    x.take(x.indexOf("//"))
  }
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
