package open5g.lib.zcpsm
import spinal.core._
import spinal.lib._
import scala.collection._

abstract class peripheralExt {
  def getName : String
  def applyIt(core : ZcpsmCore, port:Int) : Area
  var port : Int = -1
  def hasEBus : Boolean
}

class zcpsmExt(AWidth:Int,eBusName:String="ParaMem") extends peripheralExt {
  def getName = "zcpsmExt"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralExtension(AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}

class zcpsmMemSmall(BW:Int, AWidth:Int,Depth:Int,eBusName:String="ParaMem") 
  extends peripheralExt {
  def getName = "zcpsmMemSmall"
  def hasEBus = false
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralMemSmall(BW, AWidth,Depth,eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmMemBig(BW:Int, AWidth:Int,Depth:Int,eBusName:String="NocCfg") 
  extends peripheralExt {
  def getName = "zcpsmMemBig"
  def hasEBus = false
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralMemBig(BW, AWidth,Depth,eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmMemIn(BW:Int, AWidth:Int,Depth:Int,eBusName:String="Ingress") 
  extends peripheralExt {
  def getName = "zcpsmMemIn"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralMemIn(BW, AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmMemRegOut(BW:Int, AWidth:Int,Depth:Int,eBusName:String="Egress") 
  extends peripheralExt {
  def getName = "zcpsmMemRegOut"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralMemRegOut(BW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmMemOut(BW:Int, AWidth:Int,Depth:Int,eBusName:String="Egress") 
  extends peripheralExt {
  def getName = "zcpsmMemOut"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralMemOut(BW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmBusExt(AW:Int,DW:Int,AWidth:Int,eBusName:String="DebugIO")
  extends peripheralExt {
  def getName = "zcpsmBusExt"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralBusExt(AW,DW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmAxisMaster(BW:Int,AWidth:Int,eBusName:String="AxisOut")
  extends peripheralExt {
  def getName = "zcpsmAxisMaster"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralAxisMaster(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmAxisSlave(BW:Int,AWidth:Int,eBusName:String="AxisIn")
  extends peripheralExt {
  def getName = "zcpsmAxisSlave"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralAxisSlave(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmStreamMaster(BW:Int,AWidth:Int,eBusName:String="StreamOut")
  extends peripheralExt {
  def getName = "zcpsmStreamMaster"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralStreamMaster(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
class zcpsmStreamSlave(BW:Int,AWidth:Int,eBusName:String="StreamIn")
  extends peripheralExt {
  def getName = "zcpsmStreamSlave"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, port:Int) = new Area {
    import core._
    val eb = peripheralStreamSlave(BW,AWidth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(port) <> eb.zBus 
  }
}
case class zcpsmConfig(PWidth:Int,HWidth:Int,psm:String) {
  val AWidth = 8-HWidth
  val ext = mutable.Map[Int,peripheralExt]()
  def addperipheral(p:Int,a:peripheralExt) = ext += (p -> a)
  def all = {
    val exts = List(
      new zcpsmExt(AWidth,"GP0"),
      new zcpsmExt(AWidth,"GP1"),
      new zcpsmMemSmall(0,AWidth,64,"ParaMem"),
      new zcpsmMemBig(8,AWidth,16,"NocMem"),
      new zcpsmMemIn(0,AWidth, 64, "Ingress"),
      new zcpsmMemOut(0,AWidth, 64, "Egress0"),
      new zcpsmMemRegOut(8,AWidth, 16, "Egress1"),
      new zcpsmBusExt(2,2,AWidth,"DebugIO"),
      new zcpsmAxisMaster(1,AWidth,"AxisOut"),
      new zcpsmAxisSlave(1,AWidth,"AxisIn"),
      new zcpsmStreamMaster(1,AWidth,"StreamOut"),
      new zcpsmStreamSlave(1,AWidth,"StreamIn")
    )
    for(i <- 0 until exts.length) addperipheral(i,exts(i))
  }
}

case class ZcpsmCore(cfg:zcpsmConfig) extends Component {
  val io = new Bundle {
    val prog = slave(zcpsmIOW(cfg.PWidth,18))
  }
  val cpu = zcpsm(cfg.PWidth)
  import open5g.lib.zcpsm.tools.asmParser
  val parser = new asmParser
  val (a,_) = parser.fromFile(cfg.psm)
  def initProg = for(i <- 0 until (1<<cfg.PWidth)) yield {
    if(i < a.length) B(BigInt(a(i).toHex),18 bits) else B(0,18 bits)
  }
  val progMem = Mem(Bits(18 bits),initialContent = initProg)
  progMem.write( data    = io.prog.out_port,
                 address = io.prog.port_id.asUInt,
                 enable  = io.prog.write_strobe)
  cpu.io.prog.instruction := progMem(cpu.io.prog.address)
  val dList = cfg.ext.keys.toList
  val dec = zcpsmDecode(8,cfg.HWidth,dList)
  dec.io.busM <> cpu.io.iobus
  
  for((p,a) <- cfg.ext) {
    val eb = a.applyIt(this,p)
  }
}