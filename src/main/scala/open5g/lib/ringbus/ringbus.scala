package open5g.lib.ringbus

import spinal.core._
import spinal.lib._
import open5g.lib.common.{ShiftReg,Constant}
import scala.math.min

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
  val D  = Reg(RingBusIO(cfg))
  val Q  = Reg(RingBusIO(cfg))
  val hD = RingBusHeader(cfg)
  val hQ = RingBusHeader(cfg)
  D := busio.d
  hD.assignFromBits(D.D(cfg.headerLen-1 downto 0))
}

case class RingBusConfig( width : Int ) {
  val error_length      = 3
  val command_length    = 2
  val addr_length       = 6
  val busid_length      = 3
  val len_length        = 5
  val cs_len            = 2

  val slot              = 17

  val errBusLength      = Constant(1,error_length)
  val errIllegalAddress = Constant(2,error_length)

  val command_idle      = Constant(0,command_length)
  val command_write     = Constant(1,command_length)
  val command_read      = Constant(2,command_length)
  val command_complete  = Constant(3,command_length)

  def headerLen         = command_length + addr_length + busid_length + len_length
  def tag_length        = min(8,width - headerLen)
  def has_tag           = tag_length > 0
  def has_mem           = mem_addr_length > 0
  def tag_start         = headerLen
  def mem_addr_start    = headerLen + tag_length
  def mem_addr_length   = width - mem_addr_start

}

case class RingBusHeader(cfg:RingBusConfig) extends Bundle {
  val command = Bits(cfg.command_length bits)
  val addr    = Bits(cfg.addr_length bits)
  val busid   = Bits(cfg.busid_length bits)
  val len     = Bits(cfg.len_length bits)
  val tag     = if(cfg.has_tag) Bits(cfg.tag_length bits) else null
  val memAddr = if(cfg.has_mem) Bits(cfg.mem_addr_length bits) else null
}

case class RingBusController(cfg:RingBusConfig,Num:Int) extends Component with RingBusEP {
  when( D.f && 
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
  val delayF = ShiftReg(1,cfg.slot-Num*2-2)
  val delayD = ShiftReg(cfg.width,cfg.slot-Num*2-2) 
  when(io.sync || counter === U(cfg.slot-1)) {
    Q.f := True
    counter := U(0)
//    Q.D(cfg.headerLen-1 downto 0) := hQ.asBits
//    Q.D(cfg.width-1 downto cfg.headerLen) := D.D(cfg.width-1 downto cfg.headerLen) 
    Q.D := hQ.asBits
  } otherwise {
    Q.f := False
    counter := counter + 1
    Q.D := D.D
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
  val rx_sop =  D.f && 
      hD.busid === B(0) && 
      hD.addr  === B(Pos) && 
      !cfg.command_idle.is(hD.command)
  val tx_sop =  D.f &&
      tx.req &&
      (cfg.command_idle.is(hD.command) || rx_sop)
  when(D.f) {
    when(tx_sop) {
      Q.D := tx.d
    }.elsewhen(rx_sop) {
      // Q.D(cfg.width-1 downto cfg.headerLen) := D.D(cfg.width-1 downto cfg.headerLen)
      // Q.D(cfg.headerLen-1 downto 0) := B(0)
      Q.D := B(0)
    }.otherwise {
      Q.D := D.D
    }
    hold := tx_sop
  }.elsewhen(hold) {
    Q.D := tx.d
  }
  Q.f := D.f
  rx.d := D.D
  busio.q := Q
  tx.sop := tx_sop
  rx.sop := rx_sop
}

case class RingBus(cfg:RingBusConfig,Num:Int) extends Component {
  val controller = RingBusController(cfg,Num)
  val tx = Vec(slave(RingTxIO(cfg)),Num)
  val rx = Vec(master(RingRxIO(cfg)),Num)
  val io = new Bundle {
    val sync = in Bool
  }
  val nodes = (0 until Num).toList.map(i => RingBusNode(cfg,i))
  for(i <- 0 until Num) {
    nodes(i).tx <> tx(i)
    nodes(i).rx <>rx(i)
    if(i != Num-1) {
      nodes(i+1).busio.d := nodes(i).busio.q
    }
  }
  nodes(0).busio.d    := controller.busio.q
  controller.busio.d  := nodes(Num-1).busio.q    
  controller.io.sync  := io.sync
}

case class EPMemIN(cfg:RingBusConfig,sSize:Int,CS:Int) extends Component {
  val rb = slave(RingRxIO(cfg))
  val io = new Bundle {
    val addr  = in  UInt(sSize bits)
    val strb  = in  Bool
    val ready = in  Bool
    val valid = out Bool
    val data  = out Bits(cfg.width bits)
  }
  val D = RingBusHeader(cfg)
  val waddr = Reg(UInt(sSize bits))
  val len   = Reg(UInt(cfg.len_length bits))
  val hold  = RegInit(False)
  D.assignFromBits(rb.d)
  when(rb.sop && 
    cfg.command_write.is(D.command) &&
    D.memAddr(sSize+cfg.cs_len-1 downto sSize) === B(CS,cs_len bits)
    ) {
    waddr := D.memAddr(sSize-1 downto 0)
    len   := D.len - 1
    hold  := True 
  }.elsewhen(len>0) {
    len := len-1
    waddr := waddr + 1
  }.otherwise {
    hold := False
  }
  when(io.strb) {
    rnew      := True
    valid     := False
    raddr     := io.addr
  }.elsewhen(rnew) {
    rnew      := False
    raddr     := raddr + 1
    valid     := True
  }.elsewhen(valid && io.ready) {
    raddr     := raddr + 1
  }
  val ren = rnew || (valid && io.ready)
  val dataMem = Mem(Bits(cfg.width),(1<<sSize))
  dataMem.write(
    data    = rb.d,
    enable  = hold,
    addr    = waddr)
} io.data := dataMem.readSync(addr = raddr, enable = ren)