package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import open5g.lib.axis.{axis,axisu}

case class cmd_pkt_proc(  SR_AWIDTH : Int = 8,
  SR_DWIDTH : Int = 32,
  RB_AWIDTH : Int = 8,
  RB_USER_AWIDTH : Int = 8,
  RB_DWIDTH : Int = 64,
  USE_TIME : Boolean = true,
  FIFO_SIZE : Int = 5) extends Component {
  val wbus = master(Reg(new regBus))
  val io = new Bundle {
    val clear = in Bool
    val set_time = out Bits(64 bits)
    val rb_stb = in Bool
    val rb_data = in Bits(RB_DWIDTH bits)
    val rb_addr = out Bits(RB_AWIDTH bits)
    val cmd = slave Stream(axis(64))
    val set_has_time = out Bool
    val rb_addr_user = out Bits(RB_USER_AWIDTH bits)
    val resp = master Stream(axis(64))
    val vita_time = in Bits(64 bits)
  }

  /*
    output reg set_stb, output reg [SR_AWIDTH-1:0] set_addr, output reg [SR_DWIDTH-1:0] set_data,
  output reg [63:0] set_time, output reg set_has_time,
  input rb_stb, input [RB_DWIDTH-1:0] rb_data, output reg [RB_AWIDTH-1:0] rb_addr, output reg [RB_USER_AWIDTH-1:0] rb_addr_user
  */
  val set_has_time = RegInit(False)
  val rb_addr = Reg(Bits(RB_AWIDTH bits)) init 0
  val rb_addr_user = Reg(Bits(RB_USER_AWIDTH bits)) init 0
  val set_time = Reg(Bits(64 bits)) init 0
  io.set_has_time := set_has_time
  io.rb_addr_user := rb_addr_user
  io.set_time := set_time
  io.rb_addr := rb_addr

  val cmdfifo = StreamFifo(dataType=axis(64),depth=FIFO_SIZE)
  val cmd = Stream(axis(64))
  val int = Stream(axis(64))
  val resp_valid = RegInit(False)
  val resp_payload = Reg(axis(64))
  
  io.resp.valid := resp_valid
  io.resp.payload := resp_payload

  cmdfifo.io.push << io.cmd
  cmdfifo.io.pop >> cmd
  cmdfifo.io.flush := io.clear

  val pkt_vita_time_hold = Reg(Bits(64 bits))
  val has_time_hold = RegInit(False)
  val seqnum_hold = Reg(Bits(12 bits))
  val src_sid_hold, dst_sid_hold = Reg(Bits(16 bits))
  val int_tready = RegInit(False)

  val hdr_parser = cvita_hdr_parser(false) 
  val parser = hdr_parser.io.cvita
  hdr_parser.io.clear := io.clear
  hdr_parser.io.i << cmd
  hdr_parser.io.o >> int
  int.ready := int_tready
  val is_cmd_pkt = (parser.pkt_type ## parser.eob === B"100")
  val is_long_cmd_pkt = RegInit(False)
  val now = (io.vita_time === pkt_vita_time_hold)
  val late = (io.vita_time.asUInt > pkt_vita_time_hold.asUInt)
  val go = (if(USE_TIME) (!has_time_hold || now || late) else True)
  object CMDState extends SpinalEnum {
    val S_CMD_HEAD,S_CMD_TIME,S_CMD_DATA,S_SET_WAIT,S_READBACK,S_RESP_HEAD,S_RESP_TIME,S_RESP_DATA,S_DROP = newElement()
  }
  import CMDState._
  val state = Reg(CMDState())
  val set_rb_addr      = int.payload.data(SR_AWIDTH-1+32 downto 32) === B(nocShell.nocSRRegisters("RB_ADDR"),SR_AWIDTH bits)
  val set_rb_addr_user = int.payload.data(SR_AWIDTH-1+32 downto 32) === B(nocShell.nocSRRegisters("RB_ADDR_USER"),SR_AWIDTH bits)
  val hd = new cVitaHdr
  hd.pkt_type := B"11"
  hd.eob := False
  hd.has_time := Bool(USE_TIME)
  hd.seqnum := seqnum_hold
  hd.length := B"16'd16"
  hd.src_sid := dst_sid_hold
  hd.dst_sid := src_sid_hold
  hd.vita_time := pkt_vita_time_hold
  val resp_time = Reg(Bits(64 bits))
  val resp_header = hd.encode(127 downto 64)
  val rb_data_hold = Reg(Bits(64 bits))
  switch(state) {
    is(S_CMD_HEAD)  {int_tready := True}
    is(S_CMD_TIME)  {int_tready := True}
    is(S_CMD_DATA)  {int_tready := go}
    is(S_DROP)      {int_tready := True}
    default         {int_tready := False}
  }
  when(io.clear) {
    state               := S_CMD_HEAD
    resp_valid          := False
    wbus.set_stb        := False
    set_has_time     := False
    rb_addr          := B(0)
    rb_addr_user     := B(0)
  } otherwise {
    switch(state) {
      is(S_CMD_HEAD) {
        resp_valid := False
        resp_payload.last := False
        wbus.set_stb := False
        when(int.valid) {
          has_time_hold := hd.has_time
          seqnum_hold   := hd.seqnum
          src_sid_hold  := hd.src_sid
          dst_sid_hold  := hd.dst_sid
          when(is_cmd_pkt) {
            when(hd.has_time) {
              state := S_CMD_TIME
            } otherwise {
              pkt_vita_time_hold  := B"64'd0"
              state               := S_CMD_DATA
            }
          } otherwise {
            when(!int.payload.last) {
              state := S_DROP
            }
          } 
        }
      }
      is(S_CMD_TIME) {
        when(int.valid) {
          when(int.payload.last) {
            state               := S_CMD_HEAD
          } otherwise {
            pkt_vita_time_hold  := hd.vita_time
            state               := S_CMD_DATA
          }
        }
      }
      is(S_CMD_DATA) {
        when(int.valid && go) {
          is_long_cmd_pkt := !int.payload.last
          wbus.set_addr   := int.payload.data(SR_AWIDTH-1+32 downto 32)
          wbus.set_data   := int.payload.data(SR_DWIDTH-1 downto 0)
          set_time     := pkt_vita_time_hold
          set_has_time := has_time_hold
          when(set_rb_addr) {
            rb_addr       := int.payload.data(RB_AWIDTH-1 downto 0)
          }.elsewhen(set_rb_addr_user) {
            rb_addr_user  := int.payload.data(RB_USER_AWIDTH-1 downto 0)
          }
          wbus.set_stb := True
          when(int.payload.last) {
            state         := S_SET_WAIT
          } otherwise {
            state         := S_CMD_DATA
          }
        }
      }
      is(S_SET_WAIT) {
        wbus.set_stb := True
        state      := S_READBACK
      }
      is(S_READBACK) {
        when(io.rb_stb) {
          resp_time             := io.vita_time
          rb_data_hold          := io.rb_data
          resp_payload.last     := False
          resp_payload.data     := resp_header
          resp_valid            := True
          state                 := S_RESP_HEAD
        }
      }
      is(S_RESP_HEAD) {
        when(io.resp.fire) {
          resp_valid := True
          if(USE_TIME) {
            resp_payload.last := False
            resp_payload.data := resp_time
            state := S_RESP_TIME
          } else {
            resp_payload.last := True
            resp_payload.data := rb_data_hold
            state := S_RESP_DATA
          }
        }
      }
      is(S_RESP_TIME) {
        when(io.resp.fire) {
          resp_payload.last  := True
          resp_payload.data  := rb_data_hold
          resp_valid := True
          state       := S_RESP_DATA
        }
      }
      is(S_RESP_DATA) {
        when(io.resp.fire) {
          resp_payload.last  := False
          resp_valid := False
          state       := S_CMD_HEAD
        }
      }
      is(S_DROP) {
        when(int.valid && int.payload.last) {
          state       := S_CMD_HEAD
        }
      }
      // default {
      //   state       := S_CMD_HEAD
      // }
    }
  }
}
