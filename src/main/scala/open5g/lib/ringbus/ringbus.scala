package open5g.lib.ringbus

import spinal.core._
import spinal.lib._
import scala.collection._
import spinal.core.internals.Misc
import open5g.lib.common.{Constant,ShiftReg}
import scala.math.min

object ringField {
  val default = ringField(
    2, // command
    6, // addr
    3, // busid
    2  // cs
  )
}

case class ringHeader(config:ringConfig) extends Bundle {
  val command = Bits(config.fieldLength.command bits)
  val addr    = Bits(config.fieldLength.addr bits)
  val busid   = Bits(config.fieldLength.busid bits)
  val len     = Bits(config.len_length bits)
  val cs      = Bits(config.fieldLength.cs bits)
  val tag     = if(config.has_tag) Bits(config.tag_length bits) else null
  val memAddr = if(config.has_mem) UInt(config.mem_addr_length bits) else null
  
  def asMaster : Unit = {
    out(command,addr,busid,len,cs)
    if(config.has_tag) out(tag)
    if(config.has_mem) out(memAddr)
  } 
  def toList  = List(command,addr,busid,len,cs,tag,memAddr).filter(_!=null)
  def toBits  = toList.reverse.reduce(_ ## _)
  def assign(b:Bits) = {
    var start = 0
    for(f <- toList) {
      f     := b(f.getWidth+start-1 downto start)
      start =  f.getWidth+start
    }
    this
  }
}

case class ringField(command:Int,addr:Int,busid:Int,cs:Int) {
  def width = command + addr + busid + cs
}

case class ringConfig(
  Bwidth      : Int,
  Slot        : Int,
  Num         : Int,
  fieldLength : ringField = ringField.default
) {
  val items = mutable.Map(
    (0 -> ringController(this))
  )
  val blkSize = (Slot-1)*Bwidth/8
  val blkAW   = log2Up(Slot-1)
  def len_length        = log2Up(Slot)
  val error_length      = 3

  val errBusLength      = Constant(1,error_length)
  val errIllegalAddress = Constant(2,error_length)

  val command_idle      = Constant(0,fieldLength.command)
  val command_write     = Constant(1,fieldLength.command)
  val command_read      = Constant(2,fieldLength.command)
  val command_complete  = Constant(3,fieldLength.command)

  def headerLen         = fieldLength.width + len_length
  def tag_length        = min(8,Bwidth - headerLen)
  def has_mem           = mem_addr_length > 0
  def tag_start         = headerLen
  def mem_addr_start    = headerLen + tag_length
  def mem_addr_length   = Bwidth - mem_addr_start
  def has_tag           = tag_length > 0
}

case class ringIO(config:ringConfig) extends Bundle with IMasterSlave {
  val flag = Bool
  val D    = Bits(config.Bwidth bits)
  def asMaster = {
    out(D,flag)
  }
}

case class txIO(config:ringConfig) extends Bundle with IMasterSlave {
  val sop = Bool
  val req = Bool
  val D   = Bits(config.Bwidth bits)
  def asMaster = {
    out(D,req)
    in(sop)
  }
}

case class rxIO(config:ringConfig) extends Bundle with IMasterSlave {
  val sop = Bool
  val D     = Bits(config.Bwidth bits)
  def asMaster = {
    out(D,sop)
  }
}

trait ringItem {
  val config : ringConfig
  val bi  =  slave(ringIO(config))
  val bo  = master(ringIO(config))
  def connect(p:Int):Unit = {
    Misc.reflect(this,(n,o) => o match {
      case ep:EndPoint => {
        ep.bi <> bi
        ep.bo <> bo
        ep.pos := U(p)
      }
      case ri:ringItem => {
        ri.bi <> bi
        ri.bo <> bo
        ri.connect(p)
      }
    })
  }
}

abstract class EndPoint(val config : ringConfig
  ) extends Component with ringItem {
  val pos = in UInt(config.fieldLength.addr bits)
  val tx  =  slave(txIO(config))
  val rx  = master(rxIO(config))
  
  val header = ringHeader(config).assign(tx.D)
  def idle   = config.command_idle.is(header.command)
  val fin    = bi.flag
  val rx_sop = fin & (header.busid === B(0)) & (header.addr.asUInt === pos) & !idle
  val tx_sop = fin & tx.req & (idle | rx_sop) 

  val hold   = RegInit(False)
  val Q      = RegInit(B(0, config.Bwidth bits))

  rx.D    := bi.D
  bo.flag := RegNext(bi.flag)

  when(bi.flag) {
    when(tx_sop) {
      Q := tx.D
    }.elsewhen(rx_sop) {
      Q := B(0, config.Bwidth bits)
    }.otherwise {
      Q := bi.D
    }
    hold := tx_sop
  }.elsewhen(hold) {
    Q := tx.D
  }
  bo.D := Q
  tx.sop := tx_sop
  rx.sop := rx_sop
}

case class ringController(config:ringConfig) extends Component with ringItem {
  val io = new Bundle {
    val sync  = in Bool
    val error = out Bool
  }
  val counter = RegInit(U(0, log2Up(config.Slot) bits))
  val header  = ringHeader(config).assign(bi.D)
  val error   = ( !config.command_idle.is(header.command)) & (
                  header.busid =/= B(0) | 
                  header.addr.asUInt > config.Num |
                  header.addr  === B(0)
                )
  val delayF  = ShiftReg(1,config.Slot-config.Num-1)
  val delayD  = ShiftReg(config.Bwidth,config.Slot-config.Num-1) 
  val Q       = Reg(ringIO(config))
  when(io.sync || counter === config.Slot-1) {
    counter := U(0)
    Q.flag := True
    Q.D    := Mux(error,B(0),bi.D)
  } otherwise {
    Q.flag  := False
    counter := counter + 1
    Q.D     := bi.D
  }
  when(counter === config.Slot-1) {
    io.error := !bi.flag | error
  }
  delayF.io.ce    := True
  delayD.io.ce    := True
  delayF.io.d(0)  := Q.flag
  delayD.io.d     := Q.D
  bo.flag         := delayF.io.q(0)
  bo.D            := delayD.io.q
}

