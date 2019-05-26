package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import open5g.lib.axis.{axis,axisu}

case class source_flow_control( WIDTH : Int = 64) extends Component {
  val io = new Bundle {
    val i = slave Stream(axis(64))
    val clear = in Bool
    val fc = slave Stream(axis(64))
    val o = master Stream(axis(64))
    val busy = out Bool
  }
  val wbus = slave (new regBus)
  val sr_enable = setting_reg(nocShell.nocSRRegisters("FLOW_CTRL_EN"),8,4,0)
  sr_enable.wbus <> wbus
  val window_reset = sr_enable.io.changed
  val sfc_enable = sr_enable.io.o(0)
  val window_enable = sr_enable.io.o(1)
  val pkt_limit_enable = sr_enable.io.o(2)
  val fc_ack_disable = sr_enable.io.o(3)
  val sr_window_size = setting_reg(nocShell.nocSRRegisters("FLOW_CTRL_WINDOW_SIZE"),8,32,0)
  sr_window_size.wbus <> wbus
  val window_size = sr_window_size.io.o.asUInt
  val sr_pkt_limit = setting_reg(nocShell.nocSRRegisters("FLOW_CTRL_PKT_LIMIT"),8,16,0)
  sr_pkt_limit.wbus <> wbus
  val pkt_limit = sr_pkt_limit.io.o.asUInt

  val last_pkt_consumed = Reg(UInt(16 bits))
  val last_byte_consumed = Reg(UInt(32 bits))
  val window_reseting = RegInit(True)
  val window_reset_cnt = Reg(UInt(12 bits))
  
  val fc_ack_cnt = Reg(UInt(6 bits)) init 0
  val o_valid = RegInit(False)
  val o_payload = Reg(axis(64))
  val i_ready = RegInit(False)
  
  io.o.payload := o_payload
  io.o.valid := o_valid
  io.i.ready := i_ready

  when(io.clear || window_reset) {
    window_reseting  := True
    window_reset_cnt := 0
  }.elsewhen(window_reseting && !io.i.valid) {
    window_reset_cnt := window_reset_cnt + 1
    window_reseting := (window_reset_cnt === U"12'hFFF")
  }
  val cvita_hdr_parser_fc = cvita_hdr_parser(false)
  cvita_hdr_parser_fc.io.i.payload := io.fc.payload
  cvita_hdr_parser_fc.io.i.valid   := io.fc.valid
  cvita_hdr_parser_fc.io.o.ready := False
  // io.fc.ready := cvita_hdr_parser_fc.io.o.ready  
  val fc = cvita_hdr_parser_fc.io.cvita
  val is_fc_resp_pkt = ((fc.pkt_type ## fc.eob) === B(nocShell.pktType("FC_RESP"),3 bits))
  val fc_ack_dst_sid, fc_ack_src_sid, fc_resp_dst_sid, fc_resp_src_sid = Reg(Bits(16 bits)) init 0
  object SFCState extends SpinalEnum {
    val SFC_HEAD, SFC_TIME, SFC_BODY, SFC_DUMP = newElement()
  }
  val sfc_state = Reg(SFCState())
  import SFCState._
  when(io.clear || window_reseting) {
    last_pkt_consumed     := 0
    last_byte_consumed    := 0
    sfc_state             := SFC_HEAD
  } otherwise {

  }
  switch(sfc_state) {
    is(SFC_HEAD) {
      when(io.fc.fire) {
        when(io.fc.payload.last) {
          sfc_state := SFC_HEAD
        }.elsewhen(!is_fc_resp_pkt) {
          sfc_state := SFC_DUMP
        }.otherwise {
          fc_resp_dst_sid := fc.dst_sid
          fc_resp_src_sid := fc.src_sid
          when(fc.has_time) {
            sfc_state := SFC_TIME
          } otherwise {
            sfc_state := SFC_BODY
          }
        }
      }
    }
    is(SFC_TIME) {
      when(io.fc.fire) {
        when(io.fc.payload.last) {
          sfc_state := SFC_HEAD
        } otherwise {
          sfc_state := SFC_BODY
        }
      }
    }
    is(SFC_BODY) {
      when(io.fc.fire) {
        when(io.fc.payload.last) {
          fc_ack_src_sid        := fc_resp_dst_sid
          fc_ack_dst_sid        := fc_resp_src_sid
          last_pkt_consumed     := io.fc.payload.data(47 downto 32).asUInt // 16-bit packet count is in upper 32 bits.
          last_byte_consumed    := io.fc.payload.data(31 downto 0).asUInt  // Byte count is in lower 32 bits.
          sfc_state             := SFC_HEAD
        } otherwise {
          sfc_state := SFC_DUMP
        }
      }
    }
    is(SFC_DUMP) {
      when(io.fc.fire && io.fc.payload.last) {
        sfc_state := SFC_HEAD
      }
    }
  }
  val window_end, current_byte, window_free_space = Reg(UInt(32 bits)) init(0)
  val pkt_cnt_end, current_pkt_cnt, pkts_free_space = Reg(UInt(32 bits)) init(0)
  // Calculate end byte / packet of downstream receive window buffer
  window_end        := last_byte_consumed + window_size
  pkt_cnt_end       := last_pkt_consumed.resize(32) + pkt_limit.resize(32)
  // Calculate downstream receive window buffer free space in both
  // bytes and packets. This works even with wrap around
  window_free_space := window_end - current_byte
  pkts_free_space   := pkt_cnt_end - current_pkt_cnt
  
  val cvita = cVitaHdr(io.i.payload.data ## io.i.payload.data)
  val pkt_type = cvita.pkt_type
  val pkt_len = cvita.length
  val is_data_pkt = (pkt_type === B((nocShell.pktType("DATA") >> 1),2 bits))

  object STState extends SpinalEnum {
    val ST_IDLE, ST_FC_ACK_HEAD, ST_FC_ACK_PAYLOAD, ST_DATA_PKT, ST_IGNORE = newElement()
  }
  val state = Reg(STState())
  val go = RegInit(False)
  val pkt_len_reg = Reg(UInt(32 bits)) init 0
  val fc_ack_seqnum = Reg(UInt(12 bits)) init 0
  import STState._
  when(io.clear || window_reseting) {
    go              := False
    current_pkt_cnt := 0
    current_byte    := 0
    fc_ack_seqnum   := 0
    state           := ST_IDLE
    pkt_len_reg     := 0
  } otherwise {
    switch(state) {
      is(ST_IDLE) {
        pkt_len_reg := (pkt_len & B"1111_1111_1111_1000").asUInt.resize(32)
        when(sfc_enable) {
          when(!fc_ack_disable && fc_ack_cnt =/= 0) {
            state := ST_FC_ACK_HEAD
          }.elsewhen(io.i.valid) {
            when(is_data_pkt) {
              state := ST_DATA_PKT
            } otherwise {
              state := ST_IGNORE
            }
          }
        }
      }
      is(ST_FC_ACK_HEAD) {
        go := (!window_enable || (window_free_space >= 16)) && (!pkt_limit_enable || (pkts_free_space >= 1))
        when(True) {
          current_byte    := current_byte + 16
          current_pkt_cnt := current_pkt_cnt + 1
          state           := ST_FC_ACK_PAYLOAD
        }
      }
      is(ST_FC_ACK_PAYLOAD) {
        when(io.o.fire) {
          fc_ack_seqnum := fc_ack_seqnum + 1
          go            := False
          state         := ST_IDLE
        }
      }
      is(ST_DATA_PKT) {
        go := (!window_enable || (window_free_space >= pkt_len_reg)) && (!pkt_limit_enable | (pkts_free_space >= 1))
        when(io.o.fire) {
          when(io.o.payload.last) {
            go              := False
            current_byte    := current_byte + pkt_len_reg
            current_pkt_cnt := current_pkt_cnt + 1
            state           := ST_IDLE
          }
        }
      }
      is(ST_IGNORE) {
        when(io.o.fire && io.o.payload.last) {
          state := ST_IDLE
        }
      }
    }
  }
  val fc_ack_inc = (sfc_state === SFC_BODY) && io.fc.fire && io.fc.payload.last
  val fc_ack_dec = (state === ST_FC_ACK_PAYLOAD) && io.o.fire

  when(io.clear || window_reseting) {
    fc_ack_cnt := 0
  } otherwise {
    when(fc_ack_inc && !fc_ack_dec && (fc_ack_cnt < 63)) {
      fc_ack_cnt := fc_ack_cnt + 1
    }.elsewhen(!fc_ack_inc && fc_ack_dec) {
      fc_ack_cnt := fc_ack_cnt - 1
    }
  }
  val ack = new cVitaHdr
  ack.pkt_type := B((nocShell.pktType("FC_ACK") >> 1),2 bits)
  ack.eob := Bool((nocShell.pktType("FC_ACK")&1) == 1)
  ack.has_time := False
  ack.seqnum := fc_ack_seqnum.asBits
  ack.length := B"16'd16"
  ack.src_sid := fc_ack_src_sid
  ack.dst_sid := fc_ack_dst_sid
  ack.vita_time := B(0,64 bits)

  when(window_reseting) {
    o_payload := io.i.payload
    o_valid := False
    i_ready := True
  } otherwise {
    switch(state) {
      is(ST_IDLE) {
        o_payload := io.i.payload
        o_valid := False
        i_ready := False
      }
      is(ST_FC_ACK_HEAD) {
        o_payload.data  := ack.encode(127 downto 64)
        o_payload.last  := False
        o_valid := go
        i_ready  := False
      }
      is(ST_FC_ACK_PAYLOAD) {
        o_payload.data  := current_pkt_cnt ## current_byte
        o_payload.last  := True
        o_valid := True
        i_ready  := False
      }
      is(ST_DATA_PKT) {
        o_payload := io.i.payload
        o_valid   := io.i.valid && go
        i_ready   := io.o.ready && go
      }
      default {
        o_payload := io.i.payload
        o_valid   := io.i.valid
        i_ready   := io.o.ready
      }
    }
  }
  io.busy     := window_reseting
  io.fc.ready := True // FC RESP is a non-flow controlled path, so always accept packets
}