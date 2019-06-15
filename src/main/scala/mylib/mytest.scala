package mylib

import open5g.lib.zcpsm.tools._
import open5g.lib.zcpsm._
import open5g.lib.zcpsm.plugins._
import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._


object mytest extends asmParser {
  val psm = """
    |;; 1 
    |;; 1 unsigned char m;
    |;; 2 main()
    |;; 3 {
    |;; EXPORT _main
    |_main:
    |;; 4  register char j,k;
    |;; 5  short i,l,h;
    |;; 6  k=0;
    |LOAD  s03,  00
    |;; 7  j=85;
    |LOAD  s02,  55
    |;; 8  m=0;
    |LOAD  s1F,  00
    |;; 9  if( j==1 )
    |LOAD  s00,  s02
    |SUB s00,  01
    |JUMP  NZ, L1
    |L2:
    |;; 10   {
    |;; 11     k++;
    |ADD s03,  01
    |;; 12   }
    |;; 13   else
    |;; 14   {
    |JUMP  L3
    |L1:
    |;; 15     k--;
    |SUB s03,  01
    |;; 16   }
    |;; 17   if( j==1 )
    |L3:
    |LOAD  s00,  s02
    |SUB s00,  01
    |JUMP  NZ, L4
    |L5:
    |;; 18   {
    |;; 19     k++;
    |ADD s03,  01
    |;; 20   }
    |;; 21   else
    |;; 22   {
    |JUMP  L6
    |L4:
    |;; 23     k--;
    |SUB s03,  01
    |;; 24   }
    |;; 25 }
    |L6:
    |RETURN
    |;; 26 
    """.stripMargin
  def main(args: Array[String]) {
    val ra = reg(0)
    val ib = imm(10)
    val op = add(ra,ib)

    println(op)
    ib.data = 20
    println(op)
    
    val jp = jumpc("L1")
    println(jp)
    println(psm)

    val (a,b) = fromFile(psm)
    for(i <- 0 until a.length) {
      println(a(i)+f" ;; ${a(i).toHex}%05X")
    }
    println(b)
    println(toFile)
    val bin = a.map(_.toHex)
    println(PSM.disAsm(bin))
  }
}

object mytest1 {
  def main(args:Array[String]){
    dsltest.t1
  }
}

import java.io.{File, FileWriter, BufferedWriter, PrintWriter}
import scala.io.Source

object assembler extends asmParser {
  def main(args: Array[String]) {
    val fn = args(0)
    if( new File(fn).exists ) {
      val buffer = Source.fromFile(fn)
      val moduleString = buffer.getLines
      val moduleWithOutComent = removeComment(moduleString).reduce(_+"\n"+_)
      val (a,b) = fromFile(moduleWithOutComent)
      for(i <- 0 until a.length) {
        println(a(i)+f" ;; ${a(i).toHex}%05X")
      }
      println(b)
      println(toFile)
      val bin = a.map(_.toHex)
      println(PSM.disAsm(bin))  
      println(toFile)
    }
  }
}
object MyISP {
  def main(srgs: Array[String]) {
    val cfg = zcpsmConfig(10,4,mytest.psm)
    cfg.all
    SpinalVerilog(new ZcpsmCore(cfg))
  }
}

import open5g.lib.debug.{Debugable,dbBundle,DebugUtils}

object ZcpsmSimExtBus {
  def main(srgs: Array[String]) {
    val example = ExampleBus.BusExt
    SimConfig.withWave.doSim(new ExampleBus.zcpsmForTest(example)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      val dbitem = if(dut.debug) dut.db.DebugItem else null
      var idx = 0
      while(idx < 512){
        dut.io.bus.in_port #= 0
        val addr = dut.io.debugio.port_id.toInt
        dut.io.debugio.in_port #= addr/0x55
        dut.clockDomain.waitRisingEdge()
        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        SimUtils.iowDump("debug io",dut.io.debugio)
        idx += 1
      }
    }
  }
}

object ZcpsmExampleRam {
  def main(args: Array[String]) {
    val example = if(args(0) == "small") ExampleMem.MemSmall else ExampleMem.MemBig
    SimConfig.withWave.doSim(new ExampleMem.zcpsmInternal(example)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      val dbitem = if(dut.debug) dut.db.DebugItem else null
      var idx = 0
      while(idx < 256){
        dut.io.bus.in_port #= 0
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        idx += 1
      }
    }
  }
}
object ZcpsmExampleStreamOut {
  def main(args: Array[String]) {
    val example = ExampleStream.StreamOut
    SimConfig.withWave.doSim(new ExampleStream.zcpsmStreamOut(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      val dbitem = if(dut.debug) dut.db.DebugItem else null
      var idx = 0
      var cnt = 0
      while(idx < 512){
        dut.io.bus.in_port #= cnt
        if(dut.io.bus.read_strobe.toBoolean && dut.io.bus.ce.toBoolean) {
          cnt += 1
        }
        val ready = (idx&0x3) == 0
        dut.io.sout.ready #= ready
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        if(dut.io.sout.valid.toBoolean && ready) {
          println(f"sout: ${dut.io.sout.payload.toInt}%02X")
        }
        idx += 1
      }
    }
  }
}
object ZcpsmExampleStreamIn {
  def main(args: Array[String]) {
    val example = ExampleStream.StreamIn
    SimConfig.withWave.doSim(new ExampleStream.zcpsmStreamIn(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      val dbitem = if(dut.debug) dut.db.DebugItem else null
      var idx = 0
      var cnt = 0
      while(idx < 512){
        dut.io.bus.in_port #= 0
        dut.io.sin.payload #= cnt
        val valid = (idx&0xf) == 0
        dut.io.sin.valid #= valid
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        if(dut.io.sin.ready.toBoolean && valid) {
          cnt += 1
          println(f"sin: $cnt%02X")
        }
        idx += 1
      }
    }
  }
}