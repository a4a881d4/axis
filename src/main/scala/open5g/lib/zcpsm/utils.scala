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

