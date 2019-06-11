package open5g.lib.zcpsm

import spinal.core._
import spinal.lib._
import scala.collection._
trait plugin {
  val AWidth      : Int
  def eBusFactory : Bundle
  val eBusName    : String

  val zBus = slave(zcpsmIORW(AWidth))
}
case class zcpsmMemExt(AW:Int,AWidth:Int,Depth:Int,eBusName:String="ParaMem") extends Component with plugin {
  def eBusFactory = null
  val addr = RegInit(U(0,AW*8 bits))
  when(zBus.read(AW) || zBus.written(AW)) {
    addr := addr + 1
  } otherwise {
    for(i <- 0 until AW) {
      when(zBus.written(i)) {
        addr(i*8+7 downto i*8) := zBus.out_port.asUInt
      }
    }
  }
  val pM = Mem(Bits(8 bits),Depth)
  val width = log2Up(Depth)
  zBus.in_port := pM(addr(width-1 downto 0))
  pM.write( address = addr(width-1 downto 0),
            data    = zBus.out_port,
            enable  = zBus.written(AW))
}
case class zcpsmBusExt(AW:Int,DW:Int,AWidth:Int,eBusName:String="DebugIO") extends Component with plugin {
  def eBusFactory = master(zcpsmIORW(AW*8,DW*8))
  val eBus = eBusFactory
  val wBus = zBus.toWriteOnly()
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

case class eBusConfig(AW:Int,DW:Int,port:Int)
case class eMemConfig(AW:Int,Depth:Int,port:Int)
case class egressConfig(port:Int)
case class ingressConfig(port:Int)
case class zcpsmISPConfig(PWidth:Int,
                          HWidth:Int,
                          extBus:eBusConfig,
                          port:List[Int],
                          psm:String
                          )
case class zcpsmISP(cfg:zcpsmISPConfig) extends Component {
  val outPorts = cfg.port.length
  val hasExtBus = cfg.extBus != null
  val io = new Bundle {
    val busS = Vec(master(zcpsmIORW(8-cfg.HWidth)),outPorts)
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
  val dList = if(hasExtBus) cfg.port ::: List(cfg.extBus.port,8) else cfg.port
  val dec = zcpsmDecode(8,cfg.HWidth,dList)
  dec.io.busM <> cpu.io.iobus
  for(i <- 0 until outPorts) {
    dec.io.busS(i) <> io.busS(i)
  }
  if(hasExtBus) {
    val eb = zcpsmBusExt(cfg.extBus.AW,
      cfg.extBus.DW,
      8-cfg.HWidth)
    val eBus = eb.eBusFactory //master(zcpsmIORW(cfg.extBus.AW*8,cfg.extBus.DW*8))
    eb.zBus <> dec.io.busS(outPorts)
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
  }
  {
    val eb = zcpsmMemExt(1,8-cfg.HWidth,64)
    val eBus = eb.eBusFactory
    eb.zBus <> dec.io.busS(outPorts+1)
  } 
}