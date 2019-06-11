package open5g.lib.zcpsm

import spinal.core._
import spinal.lib._
import scala.collection._

trait zcpsmIO 
case class zcpsmIORW(AWidth:Int,DWidth:Int=8) extends Bundle with IMasterSlave with zcpsmIO {
  val port_id       = Bits(AWidth bits)
  val write_strobe  = Bool
  val read_strobe   = Bool
  val out_port      = Bits(DWidth bits)
  val in_port       = Bits(DWidth bits)
  val ce            = Bool
  def asMaster = {
    out(port_id,out_port,write_strobe,read_strobe,ce)
    in(in_port)
  }

  def toReadOnly(): zcpsmIOR ={
    val ret = zcpsmIOR(AWidth,DWidth)
    ret.port_id      := port_id
    ret.read_strobe  := read_strobe
    ret.ce           := ce
    in_port          := ret.in_port
    ret
  }

  def toWriteOnly(): zcpsmIOW ={
    val ret = zcpsmIOW(AWidth,DWidth)
    ret.port_id      := port_id
    ret.write_strobe := write_strobe
    ret.out_port     := out_port
    ret.ce           := ce
    ret
  }
  def hit(id:Int) = (port_id.asUInt === id)
  def written(id:Int) = ce && write_strobe && hit(id)
  def read(id:Int) = ce && read_strobe && hit(id)
}

case class zcpsmIOW(AWidth:Int,DWidth:Int=8) extends Bundle with IMasterSlave with zcpsmIO {
  val port_id       = Bits(AWidth bits)
  val write_strobe  = Bool
  val out_port      = Bits(DWidth bits)
  val ce            = Bool
  def asMaster = {
    out(port_id,out_port,write_strobe,ce)
  }
  def Q(id:Int) = RegNextWhen(out_port,written(id))
  def written(id:Int) = ce && write_strobe && (port_id.asUInt === id)
}

case class zcpsmIOR(AWidth:Int,DWidth:Int=8) extends Bundle with IMasterSlave with zcpsmIO {
  val port_id       = Bits(AWidth bits)
  val read_strobe   = Bool
  val in_port       = Bits(DWidth bits)
  val ce            = Bool
  def asMaster = {
    out(port_id,read_strobe,ce)
    in(in_port)
  }
  def read(id:Int) = ce && read_strobe && (port_id.asUInt === id)
}

case class zcpsmProg(AWidth:Int) extends Bundle with IMasterSlave {
  val address     = UInt(AWidth bits)
  val instruction = Bits(18 bits)
  def asMaster = {
    out(address)
    in(instruction)
  }
}

case class zcpsmDecode(AWidth:Int,width:Int,used:List[Int]) extends Component {
  val num = used.length
  val io = new Bundle {
    val busM = slave(zcpsmIORW(AWidth))
    val busS = Vec(master(zcpsmIORW(AWidth-width)),num)
  }
  val port_id_H = io.busM.port_id(AWidth-1 downto AWidth-width)
  val inp = List.fill(num)(Bits(8 bits))
  for(i <- 0 until num) {
    io.busS(i).port_id      := io.busM.port_id(width-1 downto 0)
    io.busS(i).write_strobe := io.busM.write_strobe
    io.busS(i).read_strobe  := io.busM.read_strobe
    io.busS(i).out_port     := io.busM.out_port
    io.busS(i).ce           := io.busM.ce && (port_id_H === used(i))
    inp(i) := Mux(io.busM.ce && (port_id_H === used(i)),
      io.busS(i).in_port, B(0,8 bits))
  }
  io.busM.in_port := inp.reduce(_ | _)
}

case class zcpsmBusExt(AW:Int,DW:Int,AWidth:Int,base:Int) extends Component {
  val io = new Bundle {
    val zBus = slave(zcpsmIORW(AWidth))
    val eBus = master(zcpsmIORW(AW*8,DW*8))
  }
  val wBus = io.zBus.toWriteOnly()
  val inp = List.fill(DW)(Bits(8 bits))
  for(i <- 0 until AW) {
    io.eBus.port_id(i*8+7 downto i*8) := wBus.Q(base+DW+i)
  }
  for(i <- 0 until DW) {
    io.eBus.out_port(i*8+7 downto i*8) := wBus.Q(base+i)
    inp(i) := Mux(io.zBus.port_id.asUInt === base+i, io.eBus.in_port(i*8+7 downto i*8), B(0,8 bits))
  }
  io.zBus.in_port := inp.reduce(_ | _)
  io.eBus.write_strobe := RegNext(wBus.written(base+DW-1))
  io.eBus.read_strobe  := RegNext(io.zBus.read(base+DW-1))
  io.eBus.ce := RegNext(io.zBus.hit(base+DW-1))
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
    val eBus = if(hasExtBus) master(zcpsmIORW(cfg.extBus.AW*8,cfg.extBus.DW*8)) else null
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
  val dList = if(hasExtBus) cfg.port ::: List(cfg.extBus.port) else cfg.port
  val dec = zcpsmDecode(8,cfg.HWidth,dList)
  dec.io.busM <> cpu.io.iobus
  for(i <- 0 until outPorts) {
    dec.io.busS(i) <> io.busS(i)
  }
  if(hasExtBus) {
    val eb = zcpsmBusExt(cfg.extBus.AW,
      cfg.extBus.DW,
      8-cfg.HWidth,
      0)
    eb.io.zBus <> dec.io.busS(outPorts)
    io.eBus <> eb.io.eBus
  } 
}