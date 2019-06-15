package open5g.lib.zcpsm.plugins

import spinal.core._
import spinal.lib._
import open5g.lib.zcpsm._

case class peripheralExtension(AWidth:Int,eBusName:String="ParaMem") 
  extends Component with peripheral with hasEBus {
  def eBusFactory = master(zcpsmIORW(AWidth))
  val eBus = eBusFactory
  val bus = zcpsmIORW(AWidth)
  zBus <> eBus
}

case class peripheralBusExt(AW:Int,DW:Int,AWidth:Int,eBusName:String="DebugIO") 
  extends Component with peripheral with hasEBus {
  def eBusFactory = master(zcpsmIORW(AW*8,DW*8))
  val eBus = eBusFactory
  val inp = List.fill(DW)(Bits(8 bits))
  for(i <- 0 until AW) {
    eBus.port_id(i*8+7 downto i*8) := wBus.Q(DW+i)
  }
  for(i <- 0 until DW) {
    eBus.out_port(i*8+7 downto i*8) := wBus.Q(i)
    inp(i) := Mux(zBus.port_id.asUInt === i, eBus.in_port(i*8+7 downto i*8), B(0,8 bits))
  }
  zBus.in_port := inp.reduce(_ | _)
  eBus.write_strobe := RegNext(wBus.written(DW-1))
  eBus.read_strobe  := RegNext(zBus.read(DW-1))
  eBus.ce := RegNext(zBus.hit(DW-1))
}

class zcpsmExt(AWidth:Int,eBusName:String="ParaMem") extends peripheralExt {
  def getName = "zcpsmExt"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralExtension(AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus
    port = dList(decport) 
  }
}
class zcpsmBusExt(AW:Int,DW:Int,AWidth:Int,eBusName:String="DebugIO")
  extends peripheralExt {
  def getName = "zcpsmBusExt"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralBusExt(AW,DW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}

object ExampleBus {
  object BusExt extends PluginsExample {
    val code = """
      |L0:
      |LOAD   s00, 00
      |LOAD   s01, 00
      |L1:
      |CALL   READ_IO
      |OUTPUT s02, 00
      |OUTPUT s03, 01
      |ADD    s00, 55
      |ADDCY  s01, 00
      |LOAD   s04, s01
      |AND    s04, FE
      |JUMP   Z, L1
      |LOAD   s00, 00
      |LOAD   s01, 00
      |LOAD   s02, 00
      |LOAD   s03, 00
      |L2:
      |CALL   WRITE_IO
      |ADD    s02, AA
      |ADDCY  s03, 01
      |ADD    s00, 55
      |ADDCY  s01, 00
      |LOAD   s04, s01
      |AND    s04, FE
      |JUMP   Z, L2
      |JUMP   L0
      |;; address(s01,s00) data(s03,s02)
      |WRITE_IO:      
      |OUTPUT s00, 12 ;; write address low
      |OUTPUT s01, 13 ;; write address high
      |OUTPUT s02, 10 ;; write data low
      |OUTPUT s03, 11 ;; write data high
      |RETURN
      |READ_IO:
      |OUTPUT s00, 12 ;; write address low
      |OUTPUT s01, 13 ;; write address high
      |LOAD   s00, s00;; Nop
      |INPUT  s02, 10 ;; write data low
      |INPUT  s03, 11 ;; write data high
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(6,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmBusExt(2,2,config.AWidth,"DebugIO"))
  }
  import open5g.lib.debug.{Debugable,dbBundle}
  class zcpsmForTest(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val debugio = master(zcpsmIORW(16,16))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    io.debugio <> core.eBus(1).asInstanceOf[zcpsmIORW]
  }
}