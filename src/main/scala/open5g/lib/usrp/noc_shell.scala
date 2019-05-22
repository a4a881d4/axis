package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import open5g.lib.axis.axis

object nocShell {
  val nocSRRegisters = Map(
    "FLOW_CTRL_BYTES_PER_ACK"     -> 1,
    "FLOW_CTRL_WINDOW_SIZE"       -> 2,
    "FLOW_CTRL_EN"                -> 3,
    "ERROR_POLICY"                -> 4,
    "SRC_SID"                     -> 5,
    "NEXT_DST_SID"                -> 6,
    "RESP_IN_DST_SID"             -> 7,
    "RESP_OUT_DST_SID"            -> 8,
    "FLOW_CTRL_PKT_LIMIT"         -> 9,
    "RB_ADDR_USER"                -> 124,
    "CLEAR_RX_FC"                 -> 125,
    "CLEAR_TX_FC"                 -> 126,
    "RB_ADDR"                     -> 127,
    // Registers 128-255 for users
    "USER_REG_BASE"               -> 128
  ) 
  val nocRBRegisters = Map(
    "NOC_ID"               -> 0,
    "GLOBAL_PARAMS"        -> 1,
    "FIFOSIZE"             -> 2,
    "MTU"                  -> 3,
    "BLOCK_PORT_SIDS"      -> 4,
    "USER_DATA"            -> 5,
    "NOC_SHELL_COMPAT_NUM" -> 6
  )
  val pktType = Map(
    "DATA"      -> 0,
    "DATA_EOB"  -> 1,
    "FC_RESP"   -> 2,
    "FC_ACK"    -> 3,
    "CMD"       -> 4,
    "CMD_EOB"   -> 5,
    "RESP"      -> 6,
    "RESP_ERR"  -> 7
    )
}

case class noc_shell(  NOC_ID : Int = 0,
  INPUT_PORTS : Int = 1,
  OUTPUT_PORTS : Int = 1,
  USE_TIMED_CMDS : Int = 0,
  STR_SINK_FIFOSIZE : Int = 11,
  MTU : Int = 10,
  USE_GATE_MASK : Int = 0,
  BLOCK_PORTS : Int = 1,
  CMD_FIFO_SIZE : Int = 5,
  RESP_FIFO_SIZE : Int = 0) extends Component {
  val io = new Bundle {
    val bus_clk = in Bool
    val bus_rst = in Bool
    val clk = in Bool
    val str_src = Vec(slave Stream(axis(64,-1)),OUTPUT_PORTS)
    val next_dst_sid = out Vec(Bits(16 bits),OUTPUT_PORTS)
    val set_data = out Vec(Bits(32 bits),BLOCK_PORTS)
    val set_addr = out Vec(Bits(8 bits),BLOCK_PORTS)
    val set_stb = out Bits(BLOCK_PORTS bits)
    val set_time = out Vec(Bits(64 bits),BLOCK_PORTS)
    val resp_in_dst_sid = out Vec(Bits(16 bits),INPUT_PORTS)
    val rb_stb = in Bits(BLOCK_PORTS bits)
    val rb_addr = out Vec(Bits(8 bits),BLOCK_PORTS)
    val rb_data = in Vec(Bits(64 bits),BLOCK_PORTS)
    val i = slave Stream(axis(64,-1))
    val debug = out Bits(64 bits)
    val clear_tx_seqnum = out Bits(OUTPUT_PORTS bits)
    val cmdout = slave Stream(axis(64,-1))
    val set_has_time = out Bits(BLOCK_PORTS bits)
    val str_sink = Vec(master Stream(axis(64,-1)),INPUT_PORTS)
    val src_sid = out Vec(Bits(16 bits),BLOCK_PORTS)
    val resp_out_dst_sid = out Vec(Bits(16 bits),OUTPUT_PORTS)
    val reset = in Bool
    val ackin = master Stream(axis(64,-1))
    val o = master Stream(axis(64,-1))
    val vita_time = in Bits(64 bits)
  }

}

case class noc_output_port( SR_FLOW_CTRL_EN : Int = 0,
  SR_FLOW_CTRL_WINDOW_SIZE : Int = 1,
  SR_FLOW_CTRL_PKT_LIMIT : Int = 2,
  PORT_NUM : Int = 0,
  MTU : Int = 10,
  USE_GATE : Int = 0) extends Component {
  val io = new Bundle {
    val clk = in Bool
    val str_src = slave Stream(axis(64,-1))
    val clear = in Bool
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val dataout = master Stream(axis(64,-1))
    val fcin = slave Stream(axis(64,-1))
    val reset = in Bool
  }
}

case class noc_input_port(  SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  SR_ERROR_POLICY : Int = 2,
  STR_SINK_FIFOSIZE : Int = 11,
  USE_TIME : Int = 0) extends Component {
  val io = new Bundle {
    val clk = in Bool
    val clear = in Bool
    val fc = master Stream(axis(64,-1))
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val i = slave Stream(axis(64,-1))
    val reset = in Bool
    val resp_sid = in Bits(32 bits)
    val o = master Stream(axis(64,-1))
  }
}

case class axi_mux( PRIO : Int = 0,
  WIDTH : Int = 64,
  PRE_FIFO_SIZE : Int = 0,
  POST_FIFO_SIZE : Int = 0,
  SIZE : Int = 4) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val i = Vec(slave Stream(axis(WIDTH,-1)),SIZE)
    val o = master Stream(axis(WIDTH,-1))
  }
  val input = Vec(Stream(axis(WIDTH,-1)),SIZE)
  val output = Stream(axis(WIDTH,-1))
  if(PRE_FIFO_SIZE > 0) {
    val fifos = List.fill(SIZE)(StreamFifo(dataType = axis(WIDTH,-1), depth = PRE_FIFO_SIZE))
    for(i <- 0 until SIZE) {
      fifos(i).io.push << io.i(i)
      fifos(i).io.pop >> input(i)
      fifos(i).io.flush := io.clear
    }  
  } else {
    for(i <- 0 until SIZE) {
      input(i) << io.i(i)
    }
  }

  val st_port = Reg(UInt(log2Up(SIZE) bits)) init 0
  val st_active = RegInit(False)

  when(st_active) {
    when(output.payload.last && output.valid && output.ready) {
      st_active := False
      if(PRIO != 0) {
        st_port := 0
      } else {
        when(st_port === SIZE-1) {
          st_port :=0
        } otherwise {
          st_port := st_port + 1
        }
      }
    }
  } otherwise {
    when(input(st_port).valid) {
      st_active := True
    } otherwise {
      when(st_port === SIZE-1) {
        st_port :=0
      } otherwise {
        st_port := st_port + 1
      }
    }
  }

  for(i <- 0 until SIZE) {
    when(st_port === i && st_active) {
      input(i).ready := output.ready
    } otherwise {
      input(i).ready := False
    }
  }

  output.payload.data := input(st_port).payload.data
  output.payload.last := input(st_port).payload.last
  output.valid        := input(st_port).valid

  if(POST_FIFO_SIZE > 0) {
    val fifoO = StreamFifo(dataType = axis(WIDTH,-1), depth = POST_FIFO_SIZE)
    fifoO.io.push << output
    fifoO.io.pop >> io.o
    fifoO.io.flush := io.clear
  } else {
    io.o << output
  }
}

case class chdr_fifo_large( SIZE : Int = 12 ) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val i = slave Stream(axis(64,-1))
    val o = master Stream(axis(64,-1))
  }
  val fifoI = StreamFifo( dataType = axis(64,-1), depth = (1 << SIZE))
  fifoI.io.push << io.i
  fifoI.io.pop >> io.o
  fifoI.io.flush := io.clear
}

case class noc_responder( SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  SR_ERROR_POLICY : Int = 2,
  USE_TIME : Int = 0) extends Component {
  val io = new Bundle {
    val clk = in Bool
    val clear = in Bool
    val fc = master Stream(axis(64,-1))
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val i = slave Stream(axis(64,-1))
    val reset = in Bool
    val resp = master Stream(axis(64,-1))
    val o = master Stream(axis(64,-1))
  }
}

trait hasReg {
  val ODWidth : Int
  val RegAddr : Int
  val at_rest : Int
  val bus = new Bundle {
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
  }
  val setReg = setting_reg(RegAddr,ODWidth,at_reset)
  setReg.io.strobe  := bus.set_stb
  setReg.io.addr    := bus.set_addr
  setReg.io.i       := bus.set_data
  val buso          = setReg.io.o
  // Bits(ODWidth bits)
  // buso              
  val buschanged    = setReg.io.changed

}
case class flow_control_responder(  WIDTH : Int = 64,
  SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  USE_TIME : Int = 0) extends Component {
  val io = new Bundle {
    val force_fc_pkt = in Bool
    val clear = in Bool
    val fc = master Stream(axis(WIDTH,-1))
    val i = slave Stream(axis(WIDTH,-1))
    val o = master Stream(axis(WIDTH,-1))
  }
  val RegAddr = nocShell.nocSRRegisters("FLOW_CTRL_BYTES_PER_ACK")
  val at_reset = 0
  val ODWidth = 32
  val enable_consumed = buso(31)
  val bytes_per_ack = buso(30 downto 0)
  val cvita = cVitaHdr(i.payload.data ## i.payload.data)
  val flow_control = Stream(axis(64,128))
  val fc_valid = RegInit(False)
  val is_fc_ack = 
  (B(3 bits, (2 downto 1) -> cvita.pkt_type, 0 -> eob) === B(nocShell.pktType("FC_ACK"),3 bits))
  val is_data_pkt = 
  (B(3 bits, (2 downto 1) -> cvita.pkt_type, 0 -> False) === B(nocShell.pktType("DATA"),3 bits))
  val is_data_pkt_reg = RegInit(False)
  val pkt_count = Reg(Bits(16 bits)) init(0)
  val byte_count= Reg(Bits(32 bits)) init(0)
  val resp_byte_count= Reg(Bits(32 bits)) init(0)
  val fc_src_sid = Reg(Bits(16 bits)) init(0)
  val fc_dst_sid = Reg(Bits(16 bits)) init(0)
  val fc_vita_time = Reg(Bits(64 bits)) init(0)
  
  val fsm = new StateMachine{
    val ST_IDLE : State = new State with EntryPoint {
      whenIsActive {
        when(io.i.fire) {
          is_data_pkt_reg := is_data_pkt
          when(is_fc_ack || is_data_pkt) {
            fc_src_sid  := dst_sid;
            fc_dst_sid  := src_sid;
            byte_count  := byte_count + WIDTH/8
            when(io.i.payload.last) {
              goto(ST_IDLE)
            }.elsewhen(cvita.has_time) {
              goto(ST_TIME)
            }.otherwise {
              gotto(ST_PAYLOAD)
            }
          } otherwise {
            goto(ST_DUMP)
          }
        }
      }
    }
    val ST_TIME : State = new State {
      whenIsActive {
        when(io.i.fire) {
          byte_count  := byte_count + WIDTH/8
          when(io.i.payload.last) {
            goto(ST_IDLE)
          } otherwise {
            goto(ST_PAYLOAD)
          }
        }
      }
    }
    val ST_PAYLOAD : State = new State {
      whenIsActive {
        when(io.i.fire) {
          when(io.i.payload.last) {
            when(is_data_pkt_reg) {
              pkt_count  := pkt_count + 1
              byte_count := byte_count + WIDTH/8
            } otherwise {
              pkt_count  := io.i.payload.data(47 downto 32)
              byte_count := io.i.payload.data(31 downto 0)
            }
            goto(ST_IDLE)
          } otherwise {
            byte_count := byte_count + WIDTH/8
          }
        }
      }
    }
    val ST_DUMP : State = new State {
      whenIsActive {
        when(io.i.fire && io.i.payload.last) {
          goto(ST_IDLE)
        }
      }
    }
  }
  when(byte_count_since_resp >= bytes_per_ack.resized || io.force_fc_pkt) {
    fc_valid := enable_consumed
    resp_byte_count := byte_count
    resp_pkt_count := pkt_count
  }.elsewhen(flow_control.fire) {
    fc_valid := False
  }
  flow_control.valid := fc_valid
  val resp_cvita = new cVitaHdr
  
  val dump = (isActive(ST_IDLE) && !is_data_pkt) || (!isActive(ST_IDLE) && !is_data_pkt_reg);
  io.o.valid := Mux(dump, False, io.i.valid)
  io.i.ready := Mux(dump, True , io.o.ready)
  io.o.payload.data  := io.i.payload.data
  io.o.payload.last  := io.i.payload.last

  flow_control.payload.data := resp_pkt_count ## resp_byte_count
  flow_control.payload.last := Ture
  resp_cvita.pkt_type := B(nocShell.pktType("FC_RESP") >> 1, 2 bits)
  resp_cvita.eob := B(nocShell.pktType("FC_RESP") & 1, 1 bits)(0)
  resp_cvita.has_time := if(USE_TIME==0) Fasle else True
  
  cvita_hdr_encoder cvita_hdr_encoder_fc (
    .pkt_type(FC_RESP_PKT[2:1]), .eob(FC_RESP_PKT[0]), .has_time(USE_TIME[0]),
    .seqnum(12'd0),         // Don't care, handled by chdr framer
    .payload_length(16'd0), // Don't care, handled by chdr framer
    .src_sid(fc_src_sid), .dst_sid(fc_dst_sid),
    .vita_time(USE_TIME[0] ? fc_vita_time : 64'd0),
    .header(flow_ctrl_tuser));
}

case class packet_error_responder(  SR_ERROR_POLICY : Int = 1,
  USE_TIME : Int = 0) extends Component with hasReg{
  val io = new Bundle {
    val seqnum_error = out Bool
    val clear = in Bool
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val i = slave Stream(axis(64,-1))
    val resp = master Stream(axis(64,-1))
    val sid = in Bits(32 bits)
    val o = master Stream(axis(64,-1))
  }
  val RegAddr = nocShell.nocSRRegisters("ERROR_POLICY")
  val ODWidth = 4
  val at_reset = 5
  val parser = cvita_hdr_parser(true)
  val policy_wait_until_next_burst = buso(3)
  val policy_wait_until_next_packet = buso(2)
  val policy_continue = buso(1)
  val send_error_pkt = buso(0)
  val clear_error = buschanged
  val hdr_stb = Bool
  val int = Stream(axis(64,-1))
  val drop = Stream(axis(64,-1))
  val cvita = new cVitaHdr
  parser.io.cvita <> cvita
  parser.io.i << io.i
  parser.io.o >> int 
  hdr_stb := parser.io.hdr_stb
  val seqnum_expected = Reg(UInt(12 bits)) init(0)
  val seqnum_hold = Reg(UInt(12 bits)) init(0)
  val seqnum_error = (seqnum_expected =/= cvita.seqnum.asUInt) && hdr_stb
  val error = Stream(axis(64,128))
  val packet_consumed = int.valid && int.ready && int.last
  val error_tvalid, error_hold, clear_error_hold = RegInit(False)
  val first_packet = RegInit(True)
  when(seqnum_error) {
    seqnum_hold := cvita.seqnum.asUInt
    error_tvalid := send_error_pkt
    error_hold := !policy_continue
  }
  when(clear_error && error_hold) {
    clear_error_hold := True
  }
  when(error_tvalid && error.ready) {
    error_tvalid := False
  }
  when(packet_consumed){
    seqnum_expected := cvita.seqnum.asUInt + 1
    when(policy_wait_until_next_packet ||
     (cvita.eob && policy_wait_until_next_burst) || 
     clear_error_hold || clear_error){
      clear_error_hold := False
          error_hold   := False
    }
  }
  when(cvita.eob) {
    first_packet := True
  } otherwise {
    first_packet := False
  }
  error.valid := error_tvalid
  drop := int.throwWhen(False)//)
  val CODE_SEQ_ERROR = B(4,32 bits) ## B(0,4 bits) ## seqnum_expected ## B(0,4 bits) ## seqnum_hold
  val CODE_SEQ_ERROR_MIDBURST = B(32,32 bits) ## B(0,4 bits) ## seqnum_expected ## B(0,4 bits) ## seqnum_hold
  val error.payload.data = Mux(first_packet,CODE_SEQ_ERROR,CODE_SEQ_ERROR_MIDBURST)
  val error.payload.user = B"11" ## 
  (if(USE_TIME == 0) B"0" else B"0")  ##
  B"1" ## B(0,12 bits) ## B(0,16 bits) ## 
  io.sid ## cvita.vita_time
  error.payload.last := True
  val chdr_framer_resp_pkt = chdr_framer(1,64)
  chdr_framer_resp_pkt.io.i << error
  chdr_framer_resp_pkt.io.i >> io.resp
  chdr_framer_resp_pkt.io.clear := False  
}

object cVitaHdr {
  def apply(header:Bits) = {
    val r = new cVitaHdr
    r.assignFromBits(header)
    r
  }
}
class cVitaHdr extends Bundle with IMasterSlave {
  val pkt_type = Bits(2 bits)
  val has_time = Bool
  val eob = Bool
  val seqnum = Bits(12 bits)
  val length = Bits(16 bits)
  val dst_sid = Bits(16 bits)
  val src_sid = Bits(16 bits)
  val vita_time = Bits(64 bits)
  override def asMaster = out(dst_sid,seqnum,
    pkt_type,has_time,src_sid,length,eob,vita_time)  
  def encode = this.asBits
  def payload_length = Mux(has_time,length.asUInt-16, length.asUInt-8)
}
case class cvita_hdr_decoder() extends Component {
  val io = new Bundle {
    val header = in Bits(128 bits)
    val cvita = master (new cVitaHdr) 
  }
  io.cvita := cVitaHdr(io.header)
}

case class cvita_hdr_encoder() extends Component {
  val io = new Bundle {
    val header = out Bits(128 bits)
    val cvita = slave (new cVitaHdr) 
  }
  io.header := io.cvita.encode
}

case class cvita_hdr_parser(REGISTER : Boolean = true) extends Component {
  val io = new Bundle {
    val payload_length = out UInt(16 bits)
    val clear = in Bool
    val vita_time_stb = out Bool
    val hdr_stb = out Bool
    val i = slave Stream(axis(64,-1))
    val o = master Stream(axis(64,-1))
    val cvita = master (new cVitaHdr)
  }
  val header = Reg(Bits(128 bits)) init 0
  val cvita = cVitaHdr(header)

  val firstTime = RegInit(True)
  val firstLine = RegInit(True)
  val readTime  = RegInit(False)
  val output = Stream(axis(64,-1))
  
  io.cvita := cvita
  io.payload_length := cvita.payload_length
  if(REGISTER) {
    output <-/< io.i
  } else {
    output << io.i
  }
  when(output.valid && output.ready) {
    firstTime := False
    when(firstLine) {
      header(127 downto 64) := output.payload.data
      firstLine := False
      when(cVitaHdr(output.payload.data ## B(0,64 bits)).has_time && !output.payload.last) {
        readTime := True
      }
      when(readTime) {
        header(63 downto 0) := output.payload.data
        readTime := False 
      }
    }
  }
  io.hdr_stb        := RegNext(firstLine && output.valid && output.ready)
  io.vita_time_stb  := RegNext(readTime  && output.valid && output.ready)
  io.o << output
}

case class setting_reg( my_addr : Int = 0,
  awidth : Int = 8,
  width : Int = 32,
  at_reset : Int = 0) extends Component {
  val io = new Bundle {
    val i = in Bits(32 bits)
    val strobe = in Bool
    val changed = out Bool
    val o = out Bits(width bits)
    val addr = in Bits(awidth bits)
    val rst = in Bool
  }
  val output = Reg(Bits(width bits)) init(at_reset)
  val changed = RegInit(False)
  when(io.strobe && io.addr === my_addr) {
    output := io.i(width-1 downto 0)
    changed := True
  } otherwise {
    changed := False
  }
  io.o := output
  io.changed := changed
}

case class chdr_framer( SIZE : Int = 10,
  WIDTH : Int = 32,
  USE_SEQ_NUM : Int = 0) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val i = slave Stream(axis(64,128))
    val o = master Stream(axis(64,-1))
  }
}