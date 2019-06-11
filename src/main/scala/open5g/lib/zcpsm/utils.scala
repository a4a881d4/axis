package open5g.lib.zcpsm

import spinal.core._
import spinal.lib._
import scala.collection._

trait zcpsmIO 
case class zcpsmIORW(AWidth:Int) extends Bundle with IMasterSlave with zcpsmIO {
  val port_id       = Bits(AWidth bits)
  val write_strobe  = Bool
  val read_strobe   = Bool
  val out_port      = Bits(8 bits)
  val in_port       = Bits(8 bits)
  val ce            = Bool
  def asMaster = {
    out(port_id,out_port,write_strobe,read_strobe,ce)
    in(in_port)
  }

  def <<(that : zcpsmIORW) : Unit = that >> this
  def >> (that : zcpsmIORW) : Unit = {
    this.port_id      := that.port_id
    this.write_strobe := that.write_strobe
    this.read_strobe  := that.read_strobe
    this.out_port     := that.out_port
    this.ce           := that.ce
    that.in_port      := this.in_port
  }

  def <<(that : zcpsmIOW) : Unit = that >> this
  def >>(that : zcpsmIOW) : Unit = {
    this.port_id      := that.port_id
    this.write_strobe := that.write_strobe
    this.out_port     := that.out_port
    this.ce           := that.ce
  }

  def <<(that : zcpsmIOR) : Unit = that >> this
  def >> (that : zcpsmIOR) : Unit = {
    this.port_id      := that.port_id
    this.read_strobe  := that.read_strobe
    this.ce           := that.ce
    that.in_port      := this.in_port
  }

  def toReadOnly(): zcpsmIOR ={
    val ret = zcpsmIOR(AWidth)
    ret << this
    ret
  }

  def toWriteOnly(): zcpsmIOW ={
    val ret = zcpsmIOW(AWidth)
    ret << this
    ret
  }
}

case class zcpsmIOW(AWidth:Int) extends Bundle with IMasterSlave with zcpsmIO {
  val port_id       = Bits(AWidth bits)
  val write_strobe  = Bool
  val out_port      = Bits(8 bits)
  val ce            = Bool
  def asMaster = {
    out(port_id,out_port,write_strobe,ce)
  }
  def Q(id:Int) = RegNextWhen(out_port,ce && write_strobe && (port_id.asUInt === id))

  def <<(that : zcpsmIORW) : Unit = that >> this
  def >>(that : zcpsmIORW) : Unit = {
    that.port_id      := this.port_id(that.AWidth-1 downto 0)
    that.write_strobe := this.write_strobe
    that.ce           := this.ce
    that.out_port     := this.out_port
  }
  def <<(that : zcpsmIOW) : Unit = that >> this
  def >>(that : zcpsmIOW) : Unit = {
    that.port_id      := this.port_id(that.AWidth-1 downto 0)
    that.write_strobe := this.write_strobe
    that.ce           := this.ce
    that.out_port     := this.out_port
  }
}

case class zcpsmIOR(AWidth:Int) extends Bundle with IMasterSlave with zcpsmIO {
  val port_id       = Bits(AWidth bits)
  val read_strobe   = Bool
  val in_port       = Bits(8 bits)
  val ce            = Bool
  def asMaster = {
    out(port_id,read_strobe,ce)
    in(in_port)
  }
  def <<(that : zcpsmIORW) : Unit = that >> this
  def >>(that : zcpsmIORW) : Unit = {
    assert(that.AWidth <= this.AWidth)
    that.port_id      := this.port_id(that.AWidth-1 downto 0)
    that.read_strobe  := this.read_strobe
    that.ce           := this.ce
    this.in_port      := that.in_port
  }
  def <<(that : zcpsmIOR) : Unit = that >> this
  def >>(that : zcpsmIOR) : Unit = {
    assert(that.AWidth <= this.AWidth)
    that.port_id      := this.port_id(that.AWidth-1 downto 0)
    that.read_strobe  := this.read_strobe
    that.ce           := this.ce
    this.in_port      := that.in_port
  }
  def readed(id:Int) = ce && read_strobe && (port_id.asUInt === id)
  val data = mutable.Map[Int,Bits]()
  def register(id:Int,s:Bits) = data += (id -> s)
  def done = {
    for(i <- 0 until (1<<AWidth)) {
      when(port_id === i)
    }
  }
}

case class zcpsmProg(AWidth:Int) extends Bundle with IMasterSlave {
  val address     = UInt(AWidth bits)
  val instruction = Bits(18 bits)
  def asMaster = {
    out(address)
    in(instruction)
  }
}

case class zcpsmDecode(width:Int) extends Component {
  val num = (1<<width)
  val io = new Bundle {
    val cein      = Bool
    val port_id_H = in  Bits(width bits)
    val ce        = out Bits(num bits)
  }
  for(i <- 0 until num) {
    when(io.port_id_H.asUInt === i) {
      io.ce(i) := io.cein
    } otherwise {
      io.ce(i) := False
    }
  }
}

