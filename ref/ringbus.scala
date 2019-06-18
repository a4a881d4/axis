package open5g.lib.ringbus

import spinal.core._
import spinal.lib._
import open5g.lib.common.{ShiftReg,Constant}
import scala.math.min
import spinal.lib.fsm._
import open5g.lib.axis._

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
  hD.assignFromBits(D.D)
}

case class RingBusConfig( width : Int,
  val slot           : Int = 17,
  val command_length : Int = 2,
  val addr_length    : Int = 6,
  val busid_length   : Int = 3 ) {

  def len_length        = log2Up(slot)

  val error_length      = 3
  val cs_len            = 2

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

case class RingBusHeader(cfg:RingBusConfig) extends Bundle with IMasterSlave{
  val command = Bits(cfg.command_length bits)
  val addr    = UInt(cfg.addr_length bits)
  val busid   = Bits(cfg.busid_length bits)
  val len     = UInt(cfg.len_length bits)
  val tag     = if(cfg.has_tag) Bits(cfg.tag_length bits) else null
  val memAddr = if(cfg.has_mem) UInt(cfg.mem_addr_length bits) else null
  def asMaster : Unit = {
    out(command,addr,busid,len)
    if(cfg.has_tag) out(tag)
    if(cfg.has_mem) out(memAddr)
  } 
}

case class RingBusController(cfg:RingBusConfig,Num:Int) extends Component with RingBusEP {
  when( D.f && 
    !cfg.command_idle.is(hD.command) && 
    (hD.addr > U(Num) || hD.busid.asUInt =/= U(0)) ) {
    hQ.assignFromBits(B(0,cfg.width bits))
  } otherwise {
    hQ := hD
  }
  val io = new Bundle {
    val sync = in Bool
  }
  val counter = Reg(UInt(log2Up(cfg.slot) bits))
  val delayF  = ShiftReg(1,cfg.slot-Num*2-2)
  val delayD  = ShiftReg(cfg.width,cfg.slot-Num*2-2) 
  when(io.sync || counter === U(cfg.slot-1)) {
    Q.f := True
    counter := U(0)
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
      hD.addr  === U(Pos) && 
      !cfg.command_idle.is(hD.command)
  val tx_sop =  D.f &&
      tx.req &&
      (cfg.command_idle.is(hD.command) || rx_sop)
  when(D.f) {
    when(tx_sop) {
      Q.D := tx.d
    }.elsewhen(rx_sop) {
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

case class EPMemIn(cfg:RingBusConfig,wSize:Int,CS:Int) extends Component {
  val rb = slave(RingRxIO(cfg))
  val io = new Bundle {
    val addr  = in  UInt(wSize bits)
    val ren   = in  Bool
    val data  = out Bits(cfg.width bits)
  }
  val D = RingBusHeader(cfg)
  val waddr = Reg(UInt(wSize bits))
  val len   = Reg(UInt(cfg.len_length bits))
  val hold  = RegInit(False)

  D.assignFromBits(rb.d)
  when(rb.sop && cfg.command_write.is(D.command) &&
    D.memAddr(wSize+cfg.cs_len-1 downto wSize) === U(CS,cfg.cs_len bits)
    ) {
    waddr := D.memAddr(wSize-1 downto 0)
    len   := D.len - 1
    hold  := True 
  }.elsewhen(len > 0) {
    len := len - 1
    waddr := waddr + 1
  }.otherwise {
    hold := False
  }
  val dataMem = Mem(Bits(cfg.width bits),(1<<wSize))
  dataMem.write(
    data    = rb.d,
    enable  = hold,
    address = waddr)
  io.data := dataMem.readSync(
    address = io.addr, 
    enable  = io.ren)
}

case class EPMemOut(cfg:RingBusConfig,wSize:Int,space:Int) extends Component {
  val rb = slave(RingTxIO(cfg))
  val io = new Bundle {
    val data   = in  Bits(cfg.width bits)
    val ren    = out Bool
    val addr   = out UInt(wSize bits)

    val laddr  = in  UInt(wSize bits)
    val header = slave(RingBusHeader(cfg))
    val req    = in  Bool

    val busy   = out Bool
  }
  
  val len   = Reg(UInt(cfg.len_length bits))
  def hasDelay = space > 0
  val delay = if(hasDelay) Reg(UInt(log2Up(space) bits)) else null
  
  val addr  = Reg(UInt(wSize bits))
  val req   = RegInit(False)
  val busy  = RegInit(False)
  val ren   = RegInit(False)
  
  io.busy := busy
  io.ren  := ren
  io.addr := addr
  rb.req  := req
  rb.d := Mux(rb.sop,io.header.asBits,io.data)
  
  val fsm = new StateMachine {
    val sIdle : State = new State with EntryPoint {
      whenIsActive {
        when(io.req) {
          goto(sPending)
        }
      }
    }
    val sPending : State = new State {
      whenIsActive {
        req   := True
        addr  := io.laddr
        len   := io.header.len
        busy  := True
        ren   := True
        goto(sReady)
      }
    }
    val sReady : State = new State {
      whenIsActive {
        when(rb.sop) {
          req  := False
          addr := addr + 1
          len  := len - 1
          goto(sTrans)
        }
      }
    }
    val sTrans : State = new State {
      whenIsActive {
        when(len > 0) {
          addr  := addr + 1
          len   := len - 1
          if(hasDelay) delay := U(space)
        } otherwise {
          busy  := False
          ren   := False
          if(hasDelay) {
            when(delay === U(0)) {
              when(io.req) {
                goto(sPending)
              } otherwise {
                goto(sIdle)
              }
            } otherwise {
              delay := delay - 1
            }  
          } else {
            when(io.req) {
              goto(sPending)
            } otherwise {
              goto(sIdle)
            }
          }
        }
      }
    }
  }
}

case class DMATask(cfg:RingBusDMAConfig) extends Bundle with IMasterSlave {
  val epaddr = UInt(cfg.bcfg.addr_length bits)
  val saddr  = UInt(cfg.swSize bits)
  val daddr  = UInt(cfg.dwSize bits)
  val len    = UInt(cfg.lwSize bits)
  def asMaster : Unit = {
    out(epaddr,saddr,daddr,len)
  }
}

case class RingBusDMAConfig(  bcfg    : RingBusConfig,
                              swSize  : Int,
                              dwSize  : Int,
                              lwSize  : Int,
                              twSize  : Int) {

  def axisConfig = AxisConfig(bcfg.addr_length + 
    swSize + 
    dwSize + 
    lwSize )

  def max_payload = bcfg.slot - 1

}

case class DMANP(cfg:RingBusDMAConfig) extends Component {
  
  val io = new Bundle {
    val header = master(RingBusHeader(cfg.bcfg))
    val req    = out Bool
    val laddr  = out UInt(cfg.swSize bits)
    val busy   = in Bool
    val sop    = in Bool
    val clear  = in Bool
    val en     = in Bool
    val task   = slave(Axis(cfg.axisConfig,-1))
  }
  val fifoCfg  = AxisFifoConfig(cfg.axisConfig,
    -1,
    cfg.twSize,
    false,
    false
  )
  val taskFifo = AxisFifoRam(fifoCfg)
  taskFifo.io.clear := io.clear
  taskFifo.io.d <> io.task
  val taskQ = Axis(cfg.axisConfig,-1)
  taskQ <> taskFifo.io.q

  val task = DMATask(cfg)
  task.assignFromBits(taskQ.tdata)

  val header = Reg(RingBusHeader(cfg.bcfg))
  header <> io.header 

  val req   = RegInit(False)
  val ready = RegInit(False)
  val saddr = Reg(UInt(cfg.swSize bits)) init(0)
  val len   = Reg(UInt(cfg.lwSize bits)) init(0)
  io.laddr := saddr

  val fsm = new StateMachine {
    val sIdle : State = new State with EntryPoint {
      whenIsActive {
        when(taskQ.tvalid) {
          ready := True
          goto(sPending)
        } otherwise {
          ready := False
        }
        header.assignFromBits(B(0,cfg.bcfg.width bits))
        req := False
      }
    }
    val sPending : State = new State {
      whenIsActive {
        ready           := False
        req             := False
        header.addr     := task.epaddr
        header.memAddr  := task.daddr
        saddr           := task.saddr
        len             := task.len
        header.command  := cfg.bcfg.command_write.toBits
        goto(sLoading)
      }
    }
    val sLoading : State = new State {
      whenIsActive {
        when(len > cfg.max_payload) {
          header.len := U(cfg.max_payload)
        } otherwise {
          header.len := len(cfg.bcfg.len_length-1 downto 0)
        }
        when(len === U(0)) {
          goto(sEnd)
        } otherwise {
          goto(sSending)
        }
        req   := True
      }
    }
    val sSending : State = new State {
      whenIsActive {
        when(io.en && io.sop) {
          header.memAddr := header.memAddr + header.len
          saddr := saddr + header.len
          len   := len   - header.len
          req   := False
          goto(sLoading)
        }
      }
    }
    val sEnd : State = new State {
      whenIsActive {
        req := False
        goto(sIdle)
      }
    }
  }
}