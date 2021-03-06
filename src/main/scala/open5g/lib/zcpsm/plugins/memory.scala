package open5g.lib.zcpsm.plugins

import spinal.core._
import spinal.lib._
import open5g.lib.zcpsm._

trait zcpsmMem extends peripheral {
  val Depth:Int
  def width = log2Up(Depth)
  val addr  = RegInit(U(0,width bits))
  when(zBus.read(0) || zBus.written(0)) {
    addr := addr + 1
  } otherwise {
    when(zBus.written(1)) {
      addr := zBus.out_port.asUInt(width-1 downto 0)
    }
  }
  def ramA : UInt
}
trait Mem8 {
  def ramA : UInt
  val pM = Mem(Bits(8 bits),1 << ramA.getWidth)
}
trait zcpsmMemBlocked extends zcpsmMem {
  val BW:Int
  def ramA = if(BW>0) {
    val block = wBus.Q(2)(BW-1 downto 0).asUInt
    block @@ addr
  } else addr
}

trait zcpsmMemWrite extends zcpsmMem with Mem8 {
  pM.write( address = ramA,
            data    = zBus.out_port,
            enable  = zBus.written(0))
}
case class peripheralMemSmall(BW:Int, AWidth:Int,Depth:Int,eBusName:String="ParaMem") 
  extends Component with zcpsmMemBlocked with zcpsmMemWrite {
  zBus.in_port := pM(ramA)
}

case class peripheralMemBig(BW:Int, AWidth:Int, Depth:Int, eBusName:String="NocCfg") 
  extends Component with zcpsmMemBlocked with zcpsmMemWrite {
  zBus.in_port := pM.readSync(ramA)
}
case class peripheralMemIn(BW:Int, AWidth:Int, Depth:Int, eBusName:String="Ingress")
  extends Component with zcpsmMemBlocked  with hasEBus with Mem8 {
    def eBusFactory = slave(zcpsmIOW(width+BW,8))
    val eBus = eBusFactory
    zBus.in_port := pM.readSync(ramA)
    pM.write(address = eBus.port_id.asUInt,
             data    = eBus.out_port,
             enable  = eBus.write_strobe) 
}
case class peripheralMixIn(BW:Int, MW:Int, AWidth:Int, Depth:Int, eBusName:String="Ingress")
  extends Component with zcpsmMemBlocked with hasEBus {
    val MWidth = 1<<MW
    def eBusFactory = slave(zcpsmIOW(width+BW-MW,8*MWidth))
    val eBus = eBusFactory
    val pMem =Mem(Vec(Bits(8 bits),MWidth), Depth/MWidth)
    val read = pMem.readAsync(ramA(width+BW-1 downto MW))
    pMem.write(
            address = eBus.port_id.asUInt,
            data    = eBus.out_port.subdivideIn(8 bits),
            enable  = eBus.write_strobe)  
    zBus.in_port := read(ramA(MW-1 downto 0))
}
case class peripheralMixOut(BW:Int, MW:Int, AWidth:Int, Depth:Int, eBusName:String="Egress")
  extends Component with zcpsmMemBlocked with hasEBus {
    val MWidth = 1<<MW
    def eBusFactory = slave(zcpsmIOR(width+BW-MW,8*MWidth))
    val eBus = eBusFactory
    val pMem = List.fill(MWidth)(Mem(Bits(8 bits),Depth/MWidth))
    val inport = Bits(8*MWidth bits)
    for(i <- 0 until MWidth) {
      inport(8*i+7 downto 8*i) := pMem(i).readAsync(eBus.port_id.asUInt)
      pMem(i).write(address = ramA(width+BW-1 downto MW),
        data    = zBus.out_port,
        enable  = zBus.written(0) && (ramA(MW-1 downto 0) === i)
      )
    }
    eBus.in_port := inport
    zBus.in_port := B(0, 8 bits)
}
case class peripheralMixRegOut(BW:Int, MW:Int, AWidth:Int, Depth:Int, eBusName:String="Egress")
  extends Component with zcpsmMemBlocked with hasEBus {
    val MWidth = 1<<MW
    def eBusFactory = slave(zcpsmIOR(width+BW-MW,8*MWidth))
    val eBus = eBusFactory
    val pMem = List.fill(MWidth)(Mem(Bits(8 bits),Depth/MWidth))
    val inport = Bits(8*MWidth bits)
    for(i <- 0 until MWidth) {
        inport(8*i+7 downto 8*i) := pMem(i).readSync(eBus.port_id.asUInt)
        pMem(i).write(address = ramA(width+BW-1 downto MW),
        data    = zBus.out_port,
        enable  = zBus.written(0) && (ramA(MW-1 downto 0) === i)
      )
    }
    eBus.in_port := inport
    zBus.in_port := B(0, 8 bits)
}
case class peripheralMemRegOut(BW:Int, AWidth:Int, Depth:Int, eBusName:String="Egress")
  extends Component with zcpsmMemBlocked with zcpsmMemWrite with hasEBus {
    def eBusFactory = slave(zcpsmIOR(width+BW,8))
    val eBus = eBusFactory
    zBus.in_port := B(0, 8 bits)
    eBus.in_port := pM.readSync(address = eBus.port_id.asUInt,enable = eBus.read_strobe)
}
case class peripheralMemOut(BW:Int, AWidth:Int, Depth:Int, eBusName:String="Egress")
  extends Component with zcpsmMemBlocked with zcpsmMemWrite with hasEBus {
    def eBusFactory = slave(zcpsmIOR(width+BW,8))
    val eBus = eBusFactory
    zBus.in_port := B(0, 8 bits)
    eBus.in_port := pM(eBus.port_id.asUInt)
}
class zcpsmMemSmall(BW:Int, AWidth:Int,Depth:Int,eBusName:String="ParaMem") 
  extends peripheralExt {
  def getName = "zcpsmMemSmall"
  def hasEBus = false
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMemSmall(BW, AWidth,Depth,eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMemBig(BW:Int, AWidth:Int,Depth:Int,eBusName:String="NocCfg") 
  extends peripheralExt {
  def getName = "zcpsmMemBig"
  def hasEBus = false
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMemBig(BW, AWidth,Depth,eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMemIn(BW:Int, AWidth:Int,Depth:Int,eBusName:String="Ingress") 
  extends peripheralExt {
  def getName = "zcpsmMemIn"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMemIn(BW, AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMixIn(BW:Int, MW:Int, AWidth:Int,Depth:Int,eBusName:String="Ingress") 
  extends peripheralExt {
  def getName = "zcpsmMixIn"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMixIn(BW, MW, AWidth, Depth, eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMemRegOut(BW:Int, AWidth:Int,Depth:Int,eBusName:String="Egress") 
  extends peripheralExt {
  def getName = "zcpsmMemRegOut"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMemRegOut(BW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMemOut(BW:Int, AWidth:Int,Depth:Int,eBusName:String="Egress") 
  extends peripheralExt {
  def getName = "zcpsmMemOut"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMemOut(BW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMixOut(BW:Int, MW:Int, AWidth:Int,Depth:Int,eBusName:String="Egress") 
  extends peripheralExt {
  def getName = "zcpsmMixOut"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMixOut(BW,MW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
class zcpsmMixRegOut(BW:Int, MW:Int, AWidth:Int,Depth:Int,eBusName:String="Egress") 
  extends peripheralExt {
  def getName = "zcpsmMixRegOut"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMixRegOut(BW,MW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
object ExampleMem {
  object MemSmall extends PluginsExample {
    val code = """
      |LOAD   s00, 00
      |LOAD   s01, 10
      |OUTPUT s00, 00
      |L0:
      |LOAD   s01, 10
      |OUTPUT s00, 11
      |L1:
      |OUTPUT s01, 10
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, E0
      |JUMP   Z, L1
      |LOAD   s01, 00
      |OUTPUT s00, 11
      |L2:
      |INPUT  s03, 10
      |OUTPUT s03, 00
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, F0
      |JUMP   Z, L2
      |JUMP   L0
      """.stripMargin
    val config = zcpsmConfig(5,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmMemSmall(0,config.AWidth,64,"ParaMem"))
  }
  object MemBig extends PluginsExample {
    val code = """
      |L0:
      |CALL   WRITE_RAM
      |CALL   READ_RAM
      |JUMP   L0
      |WRITE_RAM:    
      |LOAD   s00, 00 ;; address
      |OUTPUT s00, 11 ;; write address
      |LOAD   s01, 10 ;; for(i=0x10;i<0x20;i++)
      |WRITE_RAM_L1:
      |OUTPUT s01, 10 ;; write i
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, E0
      |JUMP   Z, WRITE_RAM_L1
      |RETURN
      |READ_RAM:
      |LOAD   s00, 00
      |LOAD   s01, 00 ;; for(i=0;i<0x10;i++)
      |OUTPUT s00, 11 ;; write address
      |LOAD   s00, s00;; insert Nop
      |READ_RAM_L2:
      |INPUT  s03, 10 ;; read
      |OUTPUT s03, 00 ;; output
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, F0
      |JUMP   Z, READ_RAM_L2
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(5,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmMemBig(0,config.AWidth,64,"ParaMem"))
  }
  class zcpsmInternal(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
  }
  object MemIn extends PluginsExample {
    val code = """
      |L0:
      |CALL   READ_RAM
      |JUMP   L0
      |READ_RAM:
      |LOAD   s00, 00
      |LOAD   s01, 00 ;; for(i=0;i<0x10;i++)
      |OUTPUT s00, 11 ;; write address
      |LOAD   s00, s00;; insert Nop
      |READ_RAM_L2:
      |INPUT  s03, 10 ;; read
      |OUTPUT s03, 00 ;; output
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, F0
      |JUMP   Z, READ_RAM_L2
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(5,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmMemIn(0,config.AWidth,64,"ParaMem"))
  }
  class zcpsmIn(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val w   = slave(zcpsmIOW(6,8))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    io.w   <> core.eBus(1).asInstanceOf[zcpsmIOW]
  }
  object MixIn extends PluginsExample {
    val code = """
      |L0:
      |CALL   READ_RAM
      |OUTPUT s03, 01
      |JUMP   L0
      |READ_RAM:
      |LOAD   s00, 00
      |LOAD   s01, 00 ;; for(i=0;i<0x40;i++)
      |OUTPUT s00, 11 ;; write address
      |LOAD   s00, s00;; insert Nop
      |READ_RAM_L2:
      |INPUT  s03, 10 ;; read
      |OUTPUT s03, 00 ;; output
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, C0
      |JUMP   Z, READ_RAM_L2
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(5,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmMixIn(0,3,config.AWidth,64,"ParaMem"))
  }
  class zcpsmMixWidthIn(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val w   = slave(zcpsmIOW(3,64))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    io.w   <> core.eBus(1).asInstanceOf[zcpsmIOW]
  }
  object MemOut extends PluginsExample {
    val code = """
      |L0:
      |CALL   WRITE_RAM
      |JUMP   L0
      |WRITE_RAM:    
      |LOAD   s00, 00 ;; address
      |OUTPUT s00, 11 ;; write address
      |LOAD   s01, 10 ;; for(i=0x10;i<0x20;i++)
      |WRITE_RAM_L1:
      |OUTPUT s01, 10 ;; write i
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, E0
      |JUMP   Z, WRITE_RAM_L1
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(5,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
  }
  class zcpsmOut(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val r   = slave(zcpsmIOR(6,8))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    io.r   <> core.eBus(1).asInstanceOf[zcpsmIOR]
  }
  object MixOut extends PluginsExample {
    val code = """
      |L0:
      |CALL   WRITE_RAM
      |OUTPUT s01, 01
      |JUMP   L0
      |WRITE_RAM:    
      |LOAD   s00, 00 ;; address
      |OUTPUT s00, 11 ;; write address
      |LOAD   s01, 00 ;; for(i=0x0;i<0x40;i++)
      |WRITE_RAM_L1:
      |OUTPUT s01, 10 ;; write i
      |ADD    s01, 01
      |LOAD   s02, s01
      |AND    s02, C0
      |JUMP   Z, WRITE_RAM_L1
      |RETURN
      """.stripMargin
    val config = zcpsmConfig(5,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
  }
  class zcpsmMixWidthOut(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus = master(zcpsmIORW(example.config.AWidth))
      val r   = slave(zcpsmIOR(3,64))
    }
    io.bus <> core.eBus(0).asInstanceOf[zcpsmIORW]
    io.r   <> core.eBus(1).asInstanceOf[zcpsmIOR]
  }
} 
  
  
  
