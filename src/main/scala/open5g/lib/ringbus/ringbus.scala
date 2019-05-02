package open5g.lib.ringbus

import spinal.core._
import spinal.lib._
import open5g.lib.common.ShiftReg

case class constant(value:Int,size:Int) {
  def toBits = B(value,size bits)
  def is(b:Bits) = b === toBits
}

trait RingBusEP {
  val cfg : RingBusConfig
  val busio = new Bundle {
    val D    = in Bits(cfg.width bits)
    val Q    = out Bits(cfg.width bits)
    val fin  = in Bool
    val fout = out Bool
  } 
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

  // len ## busid ## addr ## command

  val tag_start         = 16
  val addr_start        = 64
  def headerLen         = command_length + addr_length + busid_length + len_length
  /*
  constant state_IDLE   : natural := 0;
  constant state_PENDING  : natural := 1;
  constant state_LOADING  : natural := 2;
  constant state_READY  : natural := 3;
  constant state_ADDR   : natural := 4;
  constant state_DATA   : natural := 5;
  constant state_trans  : natural := 6;
  constant state_RECV   : natural := 7;
  constant state_SENDING  : natural := 8;
  constant state_END    : natural := 9;
  */
}

case class RingBusHeader(cfg:RingBusConfig) extends Bundle {
  val command = Bits(cfg.command_length bits)
  val addr    = Bits(cfg.addr_length bits)
  val busid   = Bits(cfg.busid_length bits)
  val len     = Bits(cfg.len_length bits)
}

case class RingBusControler(cfg:RingBusConfig,Num:Int) extends Component with RingBusEP {
  val D = RingBusHeader(cfg)
  val Q = RingBusHeader(cfg)
  D.assignFromBits(busio.D(cfg.headerLen-1 downto 0))
  when( busio.fin && 
    !cfg.command_idle.is(D.command) && 
    (D.addr.asUInt > U(Num) || D.busid.asUInt =/= U(0)) ) {
    Q.assignFromBits(B(0,cfg.headerLen bits))
  } otherwise {
    Q := D
  }
  val outD = Reg(Bits(cfg.width bits)) init 0
  val io = new Bundle {
    val sync = in Bool
  }
  val counter = Reg(UInt(log2Up(cfg.slot) bits))
  val fout = RegInit(False)
  val delayF = ShiftReg(1,cfg.slot-Num)
  val delayD = ShiftReg(cfg.width,cfg.slot-Num) 
  when(io.sync || counter === U(cfg.slot-1)) {
    fout := True
    counter := U(0)
    outD(cfg.headerLen-1 downto 0) := Q.asBits
    outD(cfg.width-1 downto cfg.headerLen) := busio.D(cfg.width-1 downto cfg.headerLen) 
  } otherwise {
    fout := False
    counter := counter + 1
    outD := busio.D
  }
  delayF.io.ce    := True
  delayD.io.ce    := True
  delayF.io.d(0)  := fout
  delayD.io.d     := outD
  busio.fout      := delayF.io.q(0)
  busio.Q         := delayD.io.q
}