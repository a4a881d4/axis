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
object MyMix {
  def main(srgs: Array[String]) {
    SpinalVerilog(new peripheralMixOut(0,3,4,64))
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
object ZcpsmExampleAxis {
  def main(args: Array[String]) {
    val example = ExampleStream.Axis
    SimConfig.withWave.doSim(new ExampleStream.zcpsmAxis(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      var idx = 0
      val message = "Hello World I am Looking for Bug".split(" ").toList.map(x => x.map(_.toInt))
      var wordCnt = 0
      var charCnt = 0
      var outMessage = ""
      while(idx < 1024){
        dut.io.bus.in_port #= 0
        val aChar = message(wordCnt)(charCnt)
        dut.io.ain.payload.data #= aChar
        val last = charCnt == message(wordCnt).length-1
        dut.io.ain.payload.last #= last
        dut.io.aout.ready #= true
        val valid = (idx&0xf) == 0
        dut.io.ain.valid #= valid
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        if(dut.io.ain.ready.toBoolean && valid) {
          if(last) {
            if(wordCnt == message.length-1) wordCnt=0 else wordCnt += 1
            charCnt = 0
          } else {
            charCnt += 1
          }
          println("ain: ",aChar.toChar)
        }
        idx += 1
        if(dut.io.aout.valid.toBoolean) {
          outMessage += dut.io.aout.payload.data.toInt.toChar
          if(dut.io.aout.payload.last.toBoolean) {
            println(outMessage)
            outMessage = ""
          }
        }
      }
    }
  }
}
object ZcpsmExampleRamIn {
  def main(args: Array[String]) {
    val example = ExampleMem.MemIn
    SimConfig.withWave.doSim(new ExampleMem.zcpsmIn(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      var idx = 0
      while(idx < 256){
        dut.io.bus.in_port #= 0
        val write_strobe = idx<64
        val out_port     = idx & 0xff
        val port_id      = idx & 0x3f
        dut.io.w.write_strobe #= write_strobe
        dut.io.w.out_port     #= out_port
        dut.io.w.port_id      #= port_id
        
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        idx += 1
      }
    }
  }
}
object ZcpsmExampleMixIn {
  def main(args: Array[String]) {
    val example = ExampleMem.MixIn
    SimConfig.withWave.doSim(new ExampleMem.zcpsmMixWidthIn(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      var idx = 0
      while(idx < 4096){
        dut.io.bus.in_port #= 0
        val write_strobe = idx < 8
        val out_port     = if(idx<8) BigInt(idx) * BigInt("123456789abcdef",16) else BigInt(0)
        val port_id      = idx & 0x7
        dut.io.w.write_strobe #= write_strobe
        dut.io.w.out_port     #= out_port
        dut.io.w.port_id      #= port_id
        
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        idx += 1
      }
    }
  }
}
object ZcpsmExampleRamOut {
  def main(args: Array[String]) {
    val example = ExampleMem.MemOut
    if(args(0) == "small") {
      example.config.addperipheral(1,new zcpsmMemOut(0,example.config.AWidth, 64, "Egress"))
    } else {
      example.config.addperipheral(1,new zcpsmMemRegOut(0,example.config.AWidth, 64, "Egress"))
    } 
    SimConfig.withWave.doSim(new ExampleMem.zcpsmOut(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      var idx = 0
      while(idx < 256){
        dut.io.bus.in_port #= 0
        val read_strobe  = true
        val port_id      = idx & 0x3f
        dut.io.r.read_strobe  #= read_strobe
        dut.io.r.port_id      #= port_id
        
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        println("read ",f"$idx%02X",f"${dut.io.r.in_port.toInt}%02X")
        idx += 1
      }
    }
  }
}
object ZcpsmExampleMixOut {
  def main(args: Array[String]) {
    val example = ExampleMem.MixOut
    if(args(0) == "small") {
      example.config.addperipheral(1,new zcpsmMixOut(0,3,example.config.AWidth, 64, "Egress"))
    } else {
      example.config.addperipheral(1,new zcpsmMixRegOut(0,3,example.config.AWidth, 64, "Egress"))
    } 
    SimConfig.withWave.doSim(new ExampleMem.zcpsmMixWidthOut(example,false)){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      var idx = 0
      while(idx < 1024){
        dut.io.bus.in_port #= 0
        val read_strobe  = true
        val port_id      = idx & 0x7
        dut.io.r.read_strobe  #= read_strobe
        dut.io.r.port_id      #= port_id
        dut.io.r.ce           #= true
        dut.clockDomain.waitRisingEdge()

        SimUtils.debugDump(dut)
        SimUtils.iowDump("bus",dut.io.bus)
        println("read ",f"$idx%02X",f"${dut.io.r.in_port.toBigInt}%X")
        idx += 1
      }
    }
  }
}