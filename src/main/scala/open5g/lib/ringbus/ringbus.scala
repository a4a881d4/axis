package open5g.lib.ringbus

import spinal.core._
import spinal.lib._
import scala.collection._
import spinal.core.internals.Misc

object ringField {
  val default = ringField(
    2, // command
    6, // addr
    3, // busid
    5, // len
    2, // cs
    8  // tag
  )
}

case class ringHeader(config:ringConfig) extends Bundle {
  val command = Bits(config.fieldLength.command bits)
  val addr    = Bits(config.fieldLength.addr bits)
  val busid   = Bits(config.fieldLength.busid bits)
  val len     = Bits(config.fieldLength.len bits)
  val cs      = Bits(config.fieldLength.cs bits)
  val tag     = Bits(config.fieldLength.tag bits)
  val dAddr   = Bits(config.Bwidth - config.fieldLength.width bits)
  def toList  = List(command,addr,busid,len,cs,tag,dAddr)
  def toBits  = toList.reduce(_ ## _)
  def assign(b:Bits) = {
    var start = 0
    for(f <- toList) {
      f := b(f.getWidth+start-1 downto start)
      start = f.getWidth+start
    }
    this
  }
}

case class ringField(command:Int,addr:Int,busid:Int,len:Int,cs:Int,tag:Int) {
  def width = command + addr + busid + len + cs + tag
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
  val phy = 8
  val vir = 24
  val blkSize = (Slot-1)*Bwidth/8
  val blkAW   = log2Up(Slot-1)
}

case class ringBundle(config:ringConfig) extends Bundle with IMasterSlave {
  val flag = Bool
  val D    = Bits(config.Bwidth bits)
  def asMaster = {
    out(D,flag)
  }
}

case class txBundle(config:ringConfig) extends Bundle with IMasterSlave {
  val tx_sop = Bool
  val Req    = Bool
  val tx     = Bits(config.Bwidth bits)
  def asMaster = {
    out(tx,Req)
    in(tx_sop)
  }
}

case class rxBundle(config:ringConfig) extends Bundle with IMasterSlave {
  val rx_sop = Bool
  val rx     = Bits(config.Bwidth bits)
  def asMaster = {
    out(rx,rx_sop)
  }
}

trait ringItem {
  val config : ringConfig
  val bi  =  slave(ringBundle(config))
  val bo  = master(ringBundle(config))
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
  val tx =  slave(txBundle(config))
  val rx = master(rxBundle(config))
  
  val header = ringHeader(config).assign(tx.tx)
  def idle   = header.command === B(0)
  val fin    = bi.flag
  val rx_sop = fin & (header.busid === B(0)) & (header.addr.asUInt === pos) & !idle
  val tx_sop = fin & tx.Req & (idle | rx_sop) 

  val hold   = RegInit(False)
  val Q      = RegInit(B(0, config.Bwidth bits))

  rx.rx  := bi.D
  bo.flag    := RegNext(bi.flag)

  when(bi.flag) {
    when(tx_sop) {
      Q := tx.tx
    }.elsewhen(rx_sop) {
      Q := B(0, config.Bwidth bits)
    }.otherwise {
      Q := bi.D
    }
    hold := tx_sop
  }.elsewhen(hold) {
    Q := tx.tx
  }
  bo.D := Q
}

case class ringController(config:ringConfig) extends Component with ringItem {
  val io = new Bundle {
    val sync  = in Bool
    val error = out Bool
  }
  val counter = RegInit(U(0, log2Up(config.Slot) bits))
  val header  = ringHeader(config).assign(bi.D)
  val error   = (header.command =/= B(0)) & (
                  header.busid =/= B(0) | 
                  header.addr.asUInt > config.Num |
                  header.addr  === B(0)
                )
  
  when(io.sync || counter === config.Slot-1) {
    counter := U(0)
    bo.flag := True
    bo.D    := Mux(error,B(0),bi.D)
  } otherwise {
    bo.flag := False
    counter := counter + 1
    bo.D    := bi.D
  }
  when(counter === config.Slot-1) {
    io.error := !bi.flag | error
  }
}

