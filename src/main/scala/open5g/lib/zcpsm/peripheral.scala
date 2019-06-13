package open5g.lib.zcpsm

import spinal.core._
import spinal.lib._
import scala.collection._

trait peripheral {
  val AWidth      : Int
  val zBus = slave(zcpsmIORW(AWidth))
  val wBus = zBus.toWriteOnly()
}
trait hasEBus {
  def eBusFactory : Bundle
  val eBusName    : String
}
trait zcpsmMem extends peripheral {
  val Depth:Int
  val width = log2Up(Depth)
  val addr  = RegInit(U(0,width bits))
  when(zBus.read(0) || zBus.written(0)) {
    addr := addr + 1
  } otherwise {
    when(zBus.written(1)) {
      addr := zBus.out_port.asUInt(width-1 downto 0)
    }
  }
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

trait zcpsmMemWrite extends zcpsmMem {
  pM.write( address = ramA,
            data    = zBus.out_port,
            enable  = zBus.written(0))
}
case class peripheralExtension(AWidth:Int,eBusName:String="ParaMem") 
  extends Component with peripheral with hasEBus {
  def eBusFactory = master(zcpsmIORW(AWidth))
  val eBus = eBusFactory
  val bus = zcpsmIORW(AWidth)
  zBus <> eBus
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
  extends Component with zcpsmMemBlocked {
    def eBusFactory = slave(zcpsmIOW(width+BW,8))
    val eBus = eBusFactory
    zBus.in_port := pM.readSync(ramA)
    pM.write(address = eBus.port_id.asUInt,
             data    = eBus.out_port,
             enable  = eBus.write_strobe) 
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

