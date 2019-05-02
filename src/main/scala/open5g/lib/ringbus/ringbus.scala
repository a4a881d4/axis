package open5g.lib.ringbus

import spinal.core._
import spinal.lib._
import open5g.lib.common.ShiftReg

case class constant(value:Int,size:Int) {
  def toBits = B(value,size bits)
  def is(b:Bits) = b === toBits
}

case class RingBusIO(cfg : RingBusConfig) extends Bundle with IMasterSlave {
  val D    = Bits(cfg.width bits)
  val f    = Bool
  def asMaster() {
    out(D,f)
  }
}
case class RingTxIO(cfg : RingBusConfig) extends Bundle with IMasterSlave {
  val d    = Bits(cfg.width bits)
  val req  = Bool
  val sop  = Bool
  def asMaster() {
    out(d,req)
    in(sop)
  }
}
case class RingRxIO(cfg : RingBusConfig) extends Bundle with IMasterSlave {
  val d    = Bits(cfg.width bits)
  val sop  = Bool
  def asMaster() {
    out(d,sop)
  }
}
trait RingBusEP {
  val cfg : RingBusConfig
  val busio = new Bundle {
    val d = slave(RingBusIO(cfg))
    val q = master(RingBusIO(cfg))
  }
  val hD = RingBusHeader(cfg)
  val hQ = RingBusHeader(cfg)
  hD.assignFromBits(busio.d.D(cfg.headerLen-1 downto 0))
  val Q = Reg(RingBusIO(cfg))
}

case class RingBusConfig( width : Int
                          ) {
  val error_length      = 3
  val command_length    = 2
  val addr_length       = 6
  val busid_length      = 3
  val len_length        = 5
  val cs_len            = 2
  val tag_length        = 8

  val slot              = 17

  val errBusLength      = constant(1,error_length)
  val errIllegalAddress = constant(2,error_length)

  val command_idle      = constant(0,command_length)
  val command_write     = constant(1,command_length)
  val command_read      = constant(2,command_length)
  val command_complete  = constant(3,command_length)

  val tag_start         = 16
  val addr_start        = 64
  def headerLen         = command_length + addr_length + busid_length + len_length

}

case class RingBusHeader(cfg:RingBusConfig) extends Bundle {
  val command = Bits(cfg.command_length bits)
  val addr    = Bits(cfg.addr_length bits)
  val busid   = Bits(cfg.busid_length bits)
  val len     = Bits(cfg.len_length bits)
}

case class RingBusController(cfg:RingBusConfig,Num:Int) extends Component with RingBusEP {
  when( busio.d.f && 
    !cfg.command_idle.is(hD.command) && 
    (hD.addr.asUInt > U(Num) || hD.busid.asUInt =/= U(0)) ) {
    hQ.assignFromBits(B(0,cfg.headerLen bits))
  } otherwise {
    hQ := hD
  }
  val io = new Bundle {
    val sync = in Bool
  }
  val counter = Reg(UInt(log2Up(cfg.slot) bits))
  val delayF = ShiftReg(1,cfg.slot-Num)
  val delayD = ShiftReg(cfg.width,cfg.slot-Num) 
  when(io.sync || counter === U(cfg.slot-1)) {
    Q.f := True
    counter := U(0)
    Q.D(cfg.headerLen-1 downto 0) := hQ.asBits
    Q.D(cfg.width-1 downto cfg.headerLen) := busio.d.D(cfg.width-1 downto cfg.headerLen) 
  } otherwise {
    Q.f := False
    counter := counter + 1
    Q.D := busio.d.D
  }
  delayF.io.ce    := True
  delayD.io.ce    := True
  delayF.io.d(0)  := Q.f
  delayD.io.d     := Q.D
  busio.q.f       := delayF.io.q(0)
  busio.q.D       := delayD.io.q
}

case class RingBusNode(cfg:RingBusConfig,Pos:Int) extends Component with RingBusEP {
  val tx = slave(RingTxIO(cfg))
  val rx = master(RingRxIO(cfg))
  val hold = RegInit(False)
  val rx_sop =  busio.d.f && 
      hD.busid === B(0) && 
      hD.addr  === B(Pos) && 
      !cfg.command_idle.is(hD.command)
  val tx_sop =  busio.d.f &&
      tx.req &&
      (cfg.command_idle.is(hD.command) || rx_sop)
  when(busio.d.f) {
    when(tx_sop) {
      Q.D := tx.d
    }.elsewhen(rx_sop) {
      Q.D(cfg.width-1 downto cfg.headerLen) := busio.d.D(cfg.width-1 downto cfg.headerLen)
      Q.D(cfg.headerLen-1 downto 0) := B(0)
    }.otherwise {
      Q.D := busio.d.D
    }
    hold := tx_sop
  }.elsewhen(hold) {
    Q.D := tx.d
  }
  Q.f := busio.d.f
  rx.d := busio.d.D
  busio.q := Q
  tx.sop := tx_sop
  rx.sop := rx_sop
}

case class RingBus(cfg:RingBusConfig,Num:Int) extends Component {
  val controller = RingBusController(cfg,Num)
  val tx = Vec(slave(RingTxIO(cfg)),Num)
  val rx = Vec(master(RingRxIO(cfg)),Num)
  val node = List.fill(Num)(RingBusNode(cfg))
}