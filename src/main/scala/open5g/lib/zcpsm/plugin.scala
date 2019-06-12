package open5g.lib.zcpsm

import spinal.core._
import spinal.lib._
import scala.collection._
trait plugin {
  val AWidth      : Int
  def eBusFactory : Bundle
  val eBusName    : String
  val zBus = slave(zcpsmIORW(AWidth))
  val wBus = zBus.toWriteOnly()
}

trait zcpsmMem extends plugin {
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
case class zcpsmExt(AWidth:Int,eBusName:String="ParaMem") extends plugin {
  def eBusFactory = master(zcpsmIORW(AWidth))
  val eBus = eBusFactory
  eBus <> zBus
}
case class zcpsmMemSmall(BW:Int, AWidth:Int,Depth:Int,eBusName:String="ParaMem") 
  extends Component with zcpsmMemBlocked with zcpsmMemWrite {
  def eBusFactory = null
  val eBus = eBusFactory
  zBus.in_port := pM(ramA)
}

case class zcpsmMemBig(BW:Int, AWidth:Int, Depth:Int, eBusName:String="NocCfg") 
  extends Component with zcpsmMemBlocked with zcpsmMemWrite {
  def eBusFactory = null
  val eBus = eBusFactory
  zBus.in_port := pM.readSync(ramA)
}
case class zcpsmMemIn(BW:Int, AWidth:Int, Depth:Int, eBusName:String="Ingress")
  extends Component with zcpsmMemBlocked {
    def eBusFactory = slave(zcpsmIOW(width,8))
    val eBus = eBusFactory
    zBus.in_port := pM.readSync(ramA)
    pM.write(address = eBus.port_id.asUInt,
             data    = eBus.out_port,
             enable  = eBus.write_strobe) 
}
case class zcpsmMemRegOut(BW:Int, AWidth:Int, Depth:Int, eBusName:String="Egress")
  extends Component with zcpsmMemBlocked with zcpsmMemWrite {
    def eBusFactory = slave(zcpsmIOR(width,8))
    val eBus = eBusFactory
    zBus.in_port := B(0, 8 bits)
    eBus.in_port := pM.readSync(address = eBus.port_id.asUInt,enable = eBus.read_strobe)
}
case class zcpsmMemOut(BW:Int, AWidth:Int, Depth:Int, eBusName:String="Egress")
  extends Component with zcpsmMemBlocked with zcpsmMemWrite {
    def eBusFactory = slave(zcpsmIOR(width,8))
    val eBus = eBusFactory
    zBus.in_port := B(0, 8 bits)
    eBus.in_port := pM(eBus.port_id.asUInt)
}
case class zcpsmBusExt(AW:Int,DW:Int,AWidth:Int,eBusName:String="DebugIO") extends Component with plugin {
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

case class zcpsmISPConfig(PWidth:Int,HWidth:Int,psm:String) {
  val AWidth = 8-HWidth
  // val ext = mutable.Map[Int,T<:plugin]()
  // def addPlugin[T<:plugin](p:Int,com:T) = ext += (p -> com)
  // def all = {
  //   val exts = List(
  //     zcpsmExt(AWidth,"GP0"),
  //     zcpsmExt(AWidth,"GP1"),
  //     zcpsmMemSmall(0,AWidth,64,"ParaMem"),
  //     zcpsmMemBig(8,AWidth,16,"NocMem"),
  //     zcpsmMemIn(0,AWidth, 64, "Ingress"),
  //     zcpsmMemOut(0,AWidth, 64, "Egress0"),
  //     zcpsmMemRegOut(8,AWidth, 16, "Egress1"),
  //     zcpsmBusExt(2,2,AWidth,"DebugIO")
  //   )
  //   for(i <- 0 until exts.length) addPlugin(i,exts(i))
  // }
  val ext:Map[Int,plugin] = Map(
    // 2 -> zcpsmMemOut(0,AWidth, 64, "Egress0")
    // 0 -> zcpsmExt(AWidth,"GP1")
    // ,
    // 1 -> zcpsmExt(AWidth,"GP1"),
    // 2 -> zcpsmMemSmall(0,AWidth,64,"ParaMem")
  ) 
}

case class zcpsmISP(cfg:zcpsmISPConfig) extends Component {
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
  val dList = List(0)
  val dec = zcpsmDecode(8,cfg.HWidth,dList)
  dec.io.busM <> cpu.io.iobus
  
  {
    val eb = zcpsmMemSmall(0,cfg.AWidth,64,"ParaMem") //zcpsmExt(cfg.AWidth,"GP0")
    if(eb.eBus != null) {
      val eBus = eb.eBusFactory
      eBus <> eb.eBus
      eBus.setName(eb.eBusName)
    }
    eb.zBus <> dec.io.busS(0)
  }
}