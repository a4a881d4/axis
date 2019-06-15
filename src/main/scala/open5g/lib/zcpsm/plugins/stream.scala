package open5g.lib.zcpsm.plugins

import spinal.core._
import spinal.lib._
import open5g.lib.zcpsm._

case class peripheralStreamMaster(BW:Int,AWidth:Int,eBusName:String="StreamOut")
  extends Component with peripheral with hasEBus {
    def eBusFactory = master(Stream(Bits(BW*8 bits)))
    val eBus = eBusFactory
    val d = Bits(BW*8 bits)
    if(BW > 1) {
      for(i <- 0 until BW-1) {
        d(i*8+7 downto i*8) := wBus.Q(i)
      }
    }
    d(BW*8-1 downto BW*8-8) := zBus.out_port
    eBus.payload := d
    eBus.valid := zBus.written(BW-1) 
    zBus.in_port(7 downto 1) := B(0,7 bits)
    zBus.in_port(0) := zBus.read(BW) & eBus.ready
}
case class peripheralStreamSlave(BW:Int,AWidth:Int,eBusName:String="StreamIn")
  extends Component with peripheral with hasEBus {
    def eBusFactory = slave(Stream(Bits(BW*8 bits)))
    val eBus = eBusFactory
    val s = eBus.halfPipe()
    val d = Reg(Bits(BW*8 bits))
    val vd = List.fill(BW+1)(Bits(8 bits))
    for(i <- 0 until BW) {
      vd(i) := Mux(zBus.read(i),d(i*8+7 downto i*8),B(0,8 bits))
    } 
    vd(BW) := B(0,7 bits) ## (zBus.read(BW) & s.valid) 
    s.ready := zBus.read(BW)
    when(s.fire) {
      d := s.payload
    }
    zBus.in_port := vd.reduce(_ | _)
}
class zcpsmStreamMaster(BW:Int,AWidth:Int,eBusName:String="StreamOut")
  extends peripheralExt {
  def getName = "zcpsmStreamMaster"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralStreamMaster(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmStreamSlave(BW:Int,AWidth:Int,eBusName:String="StreamIn")
  extends peripheralExt {
  def getName = "zcpsmStreamSlave"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralStreamSlave(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}

import open5g.lib.axis.axis
case class peripheralAxisMaster(BW:Int,AWidth:Int,eBusName:String="AxisOut")
  extends Component with peripheral with hasEBus {
    def eBusFactory = master(Stream(axis(BW*8)))
    val eBus = eBusFactory
    val d = Bits(BW*8 bits)
    if(BW > 1) {
      for(i <- 0 until BW-1) {
        d(i*8+7 downto i*8) := wBus.Q(i)
      }
    }
    d(BW*8-1 downto BW*8-8) := zBus.out_port
    eBus.payload.data := d
    eBus.payload.last := zBus.written(BW)
    eBus.valid := zBus.written(BW-1) | zBus.written(BW)
    zBus.in_port(7 downto 1) := B(0,7 bits)
    zBus.in_port(0) := zBus.read(BW) & eBus.ready
}
case class peripheralAxisSlave(BW:Int,AWidth:Int,eBusName:String="AxisIn")
  extends Component with peripheral with hasEBus {
    def eBusFactory = slave(Stream(axis(BW*8)))
    val eBus = eBusFactory
    val s = eBus.halfPipe()
    val d = Reg(Bits(BW*8 bits))
    val vd = List.fill(BW+1)(Bits(8 bits))
    for(i <- 0 until BW) {
      vd(i) := Mux(zBus.read(i),d(i*8+7 downto i*8),B(0,8 bits))
    } 
    vd(BW) := B(0,6 bits) ## Mux(zBus.read(BW),s.payload.last ## s.valid,B"00") 
    s.ready := zBus.read(BW)
    when(s.fire) {
      d := s.payload.data
    }
    zBus.in_port := vd.reduce(_ | _)
}
class zcpsmAxisMaster(BW:Int,AWidth:Int,eBusName:String="AxisOut")
  extends peripheralExt {
  def getName = "zcpsmAxisMaster"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralAxisMaster(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmAxisSlave(BW:Int,AWidth:Int,eBusName:String="AxisIn")
  extends peripheralExt {
  def getName = "zcpsmAxisSlave"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralAxisSlave(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
object ExampleStream {
    object StreamOut extends PluginsExample {
    val code = """
      |L0:
      |INPUT  s00, 00
      |CALL   WRITE_IO
      |JUMP   L0
      |;; data in s00 ;; s01 temp reg
      |WRITE_IO:      
      |Wait_ready:
      |INPUT  s01, 11
      |AND    s01, 01
      |JUMP   Z, Wait_ready
      |OUTPUT s00, 10
      |OUTPUT s00, 01
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(4,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmStreamMaster(1,config.AWidth,"StreamOut"))
  }
  class zcpsmStreamOut(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val sout = master(Stream(Bits(8 bits)))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    val buffer = StreamFifo(Bits(8 bits),2)
    buffer.io.pop  >> io.sout
    buffer.io.push << core.eBus(1).asInstanceOf[Stream[Bits]]
  }
  object StreamIn extends PluginsExample {
    val code = """
      |L0:
      |CALL   READ_IO
      |OUTPUT s00, 01
      |JUMP   L0
      |;; data in s00 ;; s01 temp reg
      |READ_IO:      
      |Wait_valid:
      |INPUT  s01, 11
      |AND    s01, 01
      |JUMP   Z, Wait_valid
      |INPUT  s00, 10
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(4,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmStreamSlave(1,config.AWidth,"StreamIn"))
  }
  class zcpsmStreamIn(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val sin = slave(Stream(Bits(8 bits)))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    io.sin >> core.eBus(1).asInstanceOf[Stream[Bits]]
  }
}