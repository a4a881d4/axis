package mylib

import open5g.lib.zcpsm.tools._
import open5g.lib.zcpsm._
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

object ZcpsmSimSmallRam {
  class zcpsmForTest extends Component with Debugable{
    val debug = false
    val psm = """
    |LOAD   s00, 00
    |LOAD   s01, 10
    |OUTPUT s00, 00
    |L0:
    |LOAD   s01, 10
    |OUTPUT s00, 11
    |L1:
    |OUTPUT s01, 10
    |ADD    s00, 01
    |ADD    s01, 01
    |LOAD   s02, s01
    |AND    s02, E0
    |JUMP   Z, L1
    |LOAD   s01, 00
    |OUTPUT s00, 10
    |L2:
    |INPUT  s03, 10
    |OUTPUT s03, 00
    |ADD    s01, 01
    |LOAD   s02, s01
    |AND    s02, F0
    |JUMP   Z, L2
    |JUMP   L0
    """.stripMargin
    val cfg = zcpsmConfig(5,4,psm)
    println(cfg.program)
    cfg.addperipheral(0,new zcpsmExt(cfg.AWidth,"GP0"))
    cfg.addperipheral(1,new zcpsmMemSmall(0,cfg.AWidth,64,"ParaMem"))
    val core = ZcpsmCore(cfg,debug)
    val io = new Bundle {
      val bus = master(zcpsmIORW(cfg.AWidth))
    }
    val dbIn = Bits(db.inAlloc bits)
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    core.io.prog.write_strobe := False
    core.io.prog.out_port := B(0,18 bits)
    core.io.prog.port_id := B(0,cfg.PWidth bits)
    val dbPort = db finalDb
  }
  def main(srgs: Array[String]) {
    SimConfig.withWave.doSim(new zcpsmForTest){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      val dbitem = if(dut.debug) dut.db.DebugItem else null
      var idx = 0
      while(idx < 128){
        dut.io.bus.in_port #= 0
        dut.clockDomain.waitRisingEdge()

        if(dut.debug) {
          val cap = DebugUtils.Capture2Signal(dut.dbPort.capture.toBigInt,dbitem)
          val ins = cap("/core/cpu/ins").intValue
          val pc = cap("/core/cpu/pc").intValue
          val instruction = cap("/core/cpu/instruction").intValue
          println(dut.io.bus.write_strobe.toBoolean,
            f"${dut.io.bus.out_port.toInt}%02x",
            f"${dut.io.bus.port_id.toInt}%02x",
            f"$ins%05x",
            f"$pc%05x",
            f"$instruction%05x"
          )
        } else {
          val write_strobe = dut.io.bus.write_strobe.toBoolean
          val out_port     = dut.io.bus.out_port.toInt
          val port_id      = dut.io.bus.port_id.toInt
          val ce           = dut.io.bus.ce.toBoolean
          if(ce && write_strobe) {
            println(
              f"${port_id}%02x",
              f"${out_port}%02x"
            )
          }
        }
        idx += 1
      }
    }
  }
}
object ZcpsmSimBigRam {
  class zcpsmForTest extends Component with Debugable{
    val debug = false
    val psm = """
    |LOAD   s00, 00 ;; address
    |L0:
    |OUTPUT s00, 11 ;; write address
    |LOAD   s01, 10 ;; for(i=0x10;i<0x20;i++)
    |L1:
    |OUTPUT s01, 10 ;; write i
    |ADD    s01, 01
    |LOAD   s02, s01
    |AND    s02, E0
    |JUMP   Z, L1
    |LOAD   s00, 00
    |LOAD   s01, 00 ;; for(i=0;i<0x10;i++)
    |OUTPUT s00, 11 ;; write address
    |L2:
    |INPUT  s03, 10 ;; read
    |OUTPUT s03, 00 ;; output
    |ADD    s01, 01
    |LOAD   s02, s01
    |AND    s02, F0
    |JUMP   Z, L2
    |JUMP   L0
    """.stripMargin
    val cfg = zcpsmConfig(5,4,psm)
    println(cfg.program)
    cfg.addperipheral(0,new zcpsmExt(cfg.AWidth,"GP0"))
    cfg.addperipheral(1,new zcpsmMemSmall(0,cfg.AWidth,64,"ParaMem"))
    val core = ZcpsmCore(cfg,debug)
    val io = new Bundle {
      val bus = master(zcpsmIORW(cfg.AWidth))
    }
    val dbIn = Bits(db.inAlloc bits)
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    core.io.prog.write_strobe := False
    core.io.prog.out_port := B(0,18 bits)
    core.io.prog.port_id := B(0,cfg.PWidth bits)
    val dbPort = db finalDb
  }
  def main(srgs: Array[String]) {
    SimConfig.withWave.doSim(new zcpsmForTest){ dut => 
      dut.clockDomain.forkStimulus(period = 10)
      val dbitem = if(dut.debug) dut.db.DebugItem else null
      var idx = 0
      while(idx < 256){
        dut.io.bus.in_port #= 0
        dut.clockDomain.waitRisingEdge()

        if(dut.debug) {
          val cap = DebugUtils.Capture2Signal(dut.dbPort.capture.toBigInt,dbitem)
          val ins = cap("/core/cpu/ins").intValue
          val pc = cap("/core/cpu/pc").intValue
          val instruction = cap("/core/cpu/instruction").intValue
          println(dut.io.bus.write_strobe.toBoolean,
            f"${dut.io.bus.out_port.toInt}%02x",
            f"${dut.io.bus.port_id.toInt}%02x",
            f"$ins%05x",
            f"$pc%05x",
            f"$instruction%05x"
          )
        } else {
          val write_strobe = dut.io.bus.write_strobe.toBoolean
          val out_port     = dut.io.bus.out_port.toInt
          val port_id      = dut.io.bus.port_id.toInt
          val ce           = dut.io.bus.ce.toBoolean
          if(ce && write_strobe) {
            println(
              f"${port_id}%02x",
              f"${out_port}%02x"
            )
          }
        }
        idx += 1
      }
    }
  }
}