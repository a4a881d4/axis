package open5g.lib.axis

import spinal.core._
import spinal.lib._

case class AxisFifoConfig( acfg : AxisConfig,
                      userWidth : Int,
                      wSize     : Int,
                      useState  : Boolean = true,
                      useAddr   : Boolean = false) {
  def nSize = (1 << wSize)+1
  def sSize = log2Up(nSize)
}

trait AxisFifoIO {
  val cfg : AxisFifoConfig
  var io = new Bundle {
    val clear = in Bool
    val d = slave(Axis(cfg.acfg,cfg.userWidth))
    val q = master(Axis(cfg.acfg,cfg.userWidth)) 
  }
  val state = if (cfg.useState) new Bundle {
    val space    = out UInt(cfg.sSize bits)
    val occupied = out UInt(cfg.sSize bits)
  } else null
  val addr = if (cfg.useAddr) new Bundle {
    val read  = out UInt(cfg.wSize bits)
    val write = out UInt(cfg.wSize bits)
  } else null
}

case class AxisFifoFlop(cfg:AxisFifoConfig) extends Component with AxisFifoIO {

  assert(cfg.wSize == 0, s"fifo flop wSize != 0")

  val i_tready = Bool
  val buf = Reg(Axis(cfg.acfg,cfg.userWidth))

  i_tready := !io.clear & (!buf.tvalid | io.q.tready)
  when(io.clear) {
    buf.tvalid := False
  } otherwise {
    buf.tvalid := (i_tready & io.d.tvalid) | (buf.tvalid & !io.q.tready)
  }
  
  when(io.d.tvalid & i_tready) {
    io.d ddrive buf
  }

  io.d.tready := i_tready

  buf drive io.q

  if(cfg.useState) {
    state.space(0)    := i_tready
    state.occupied(0) := buf.tvalid
  }
}

case class AxisFifoFlop2(cfg:AxisFifoConfig)  extends Component with AxisFifoIO {

  assert(cfg.wSize == 1, s"fifo flop 2 wSize != 1")

  val buf  = Reg(Axis(cfg.acfg,cfg.userWidth))
  val temp = Reg(Axis(cfg.acfg,cfg.userWidth))
  when(io.clear) {
    buf.tvalid  := False
    temp.tvalid := False
  } otherwise {
    when(!buf.tvalid | io.q.tready) {
      when(temp.tvalid) {
        temp ddrive buf
        buf.tvalid := True
      } otherwise {
        io.d drive buf
      }
      temp.tvalid := False
    } otherwise {
      when(!temp.tvalid) {
        io.d drive temp
      }
    }
  }

  io.d.tready := temp.tvalid

  buf drive io.q

  if(cfg.useState) {
    val occupied = (temp.tvalid ? U(1,2 bits) | U(0,2 bits)) + (buf.tvalid ? U(1,2 bits) | U(0,2 bits))
    state.space := U(2) - occupied
    state.occupied := occupied
  }
}

case class AxisFifoRam(cfg:AxisFifoConfig)  extends Component with AxisFifoIO {

  val buf = Axis(cfg.acfg,cfg.userWidth)
  val o = Reg(Axis(cfg.acfg,cfg.userWidth))
  val full = Bool
  val empty = Bool

  val write     = io.d.tvalid & (!full)
  val read_int  = !empty & buf.tready
  val read      = io.q.tready & o.tvalid
  val wr_addr   = Reg(UInt(cfg.wSize bits)) init 0
  val rd_addr   = Reg(UInt(cfg.wSize bits)) init 0
  
  io.d.tready    := !full
  val read_state = Reg(UInt(2 bits)) init 0
  val empty_reg = RegInit(True)
  val full_reg = RegInit(False)
  when(io.clear) {
    wr_addr := U(0)
  }.elsewhen(write) {
    wr_addr := wr_addr + 1
  }

  when(io.clear) {
    read_state := U(0)
    rd_addr := U(0)
    empty_reg := True
  } otherwise {
    switch(read_state) {
      is(U(0)) {
        when(write) {
          read_state := U(1)
        }
      }
      is(U(1)) {
        read_state := U(2)
        rd_addr := rd_addr + 1
        empty_reg := False
      }
      is(U(2)) {
        when(read_int) {
          when(rd_addr === wr_addr) {
            empty_reg := True
            when(write) {
              read_state := U(1)
            } otherwise {
              read_state := U(0)
            }
          } otherwise {
            rd_addr := rd_addr + 1
          }
        }  
      }
    }
  }
  val dont_write_past_me = rd_addr - U(2)
  val becoming_full = (wr_addr === dont_write_past_me)
  when(io.clear) {
    full_reg := False
  }.elsewhen(read_int & !write) {
    full_reg := False
  }.elsewhen(write & !read_int & becoming_full) {
    full_reg := True
  }

  empty := empty_reg
  full  := full_reg
  when(io.clear) {
    o.tvalid := False
  }.elsewhen(buf.tready) {
    o.tvalid := !empty
    buf ddrive o
  }
  buf.tready := io.q.tready | o.tvalid
  o drive io.q

  val dataMem = Mem(Bits(cfg.acfg.dataWidth bits), (1<<cfg.wSize))
  dataMem.write(
      enable  = write
    , address = wr_addr
    , data    = io.d.tdata
    )
  buf.tdata := dataMem.readSync(
      enable = (read_state === U(1))|read_int
    , address = rd_addr)
  
  val lastMem = Mem(Bits(1 bits), (1<<cfg.wSize))
  lastMem.write(
      enable  = write
    , address = wr_addr
    , data    = io.d.tlast.asBits
    )
  buf.tlast := lastMem.readSync(
      enable = (read_state === U(1))|read_int
    , address = rd_addr)(0)
  
  if(cfg.userWidth>=0) {
    val userMem = Mem(Bits(cfg.userWidth bits), (1<<cfg.wSize))
    userMem.write(
        enable  = write
      , address = wr_addr
      , data    = io.d.tuser
      )
    buf.tuser := userMem.readSync(
        enable = (read_state === U(1))|read_int
      , address = rd_addr)
  }
  if(cfg.useState) {

    val space = Reg(UInt(cfg.sSize bits)) init cfg.nSize
    val occupied = Reg(UInt(cfg.sSize bits)) init cfg.nSize

    when(io.clear) {
      space    := U(cfg.nSize)
      occupied := U(0)
    }.elsewhen(read & !write) {
      space := space + 1
      occupied := occupied - 1
    }.elsewhen(!read & write) {
      space := space - 1
      occupied := occupied + 1
    }
    
    state.space := space
    state.occupied := occupied
  }
  if(cfg.useAddr) {
    addr.read := rd_addr
    addr.write := wr_addr
  }
}