package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import open5g.lib.axis.{axis,axisu}

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
    val str_src = Vec(slave Stream(axis(64)),OUTPUT_PORTS)
    val next_dst_sid = out Vec(Bits(16 bits),OUTPUT_PORTS)
    val set_data = out Vec(Bits(32 bits),BLOCK_PORTS)
    val set_addr = out Vec(Bits(8 bits),BLOCK_PORTS)
    val set_stb = out Bits(BLOCK_PORTS bits)
    val set_time = out Vec(Bits(64 bits),BLOCK_PORTS)
    val resp_in_dst_sid = out Vec(Bits(16 bits),INPUT_PORTS)
    val rb_stb = in Bits(BLOCK_PORTS bits)
    val rb_addr = out Vec(Bits(8 bits),BLOCK_PORTS)
    val rb_data = in Vec(Bits(64 bits),BLOCK_PORTS)
    val i = slave Stream(axis(64))
    val debug = out Bits(64 bits)
    val clear_tx_seqnum = out Bits(OUTPUT_PORTS bits)
    val cmdout = slave Stream(axis(64))
    val set_has_time = out Bits(BLOCK_PORTS bits)
    val str_sink = Vec(master Stream(axis(64)),INPUT_PORTS)
    val src_sid = out Vec(Bits(16 bits),BLOCK_PORTS)
    val resp_out_dst_sid = out Vec(Bits(16 bits),OUTPUT_PORTS)
    val reset = in Bool
    val ackin = master Stream(axis(64))
    val o = master Stream(axis(64))
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
    val str_src = slave Stream(axis(64))
    val clear = in Bool
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val dataout = master Stream(axis(64))
    val fcin = slave Stream(axis(64))
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
    val fc = master Stream(axis(64))
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val i = slave Stream(axis(64))
    val reset = in Bool
    val resp_sid = in Bits(32 bits)
    val o = master Stream(axis(64))
  }
}

case class axi_mux( PRIO : Int = 0,
  WIDTH : Int = 64,
  PRE_FIFO_SIZE : Int = 0,
  POST_FIFO_SIZE : Int = 0,
  SIZE : Int = 4) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val i = Vec(slave Stream(axis(WIDTH)),SIZE)
    val o = master Stream(axis(WIDTH))
  }
  val input = Vec(Stream(axis(WIDTH)),SIZE)
  val output = Stream(axis(WIDTH))
  if(PRE_FIFO_SIZE > 0) {
    val fifos = List.fill(SIZE)(StreamFifo(dataType = axis(WIDTH), depth = PRE_FIFO_SIZE))
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
    val fifoO = StreamFifo(dataType = axis(WIDTH), depth = POST_FIFO_SIZE)
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
    val i = slave Stream(axis(64))
    val o = master Stream(axis(64))
  }
  val fifoI = StreamFifo( dataType = axis(64), depth = (1 << SIZE))
  fifoI.io.push << io.i
  fifoI.io.pop >> io.o
  fifoI.io.flush := io.clear
}

case class noc_responder( SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  SR_ERROR_POLICY : Int = 2,
  USE_TIME : Int = 0) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val resp_sid = in Bits(32 bits)
    val fc = master Stream(axis(64))
    val i = slave Stream(axis(64))
    val resp = master Stream(axis(64))
    val o = master Stream(axis(64))
  }
  val bus = slave(new regBus)
  val respFC = flow_control_responder(SR_FLOW_CTRL_BYTES_PER_ACK=SR_FLOW_CTRL_BYTES_PER_ACK,USE_TIME=USE_TIME)
  val respPE = packet_error_responder(SR_ERROR_POLICY=SR_ERROR_POLICY,USE_TIME=USE_TIME)
  bus <> respPE.bus
  bus <> respFC.bus
  val int = Stream(axis(64))
  respFC.io.force_fc_pkt := respPE.io.seqnum_error
  respFC.io.i << io.i
  io.fc << respFC.io.fc
  int << respFC.io.o
  respPE.io.i << int
  io.o << respPE.io.o
  io.resp << respPE.io.resp
  respPE.io.sid := io.resp_sid
}

class regBus extends Bundle with IMasterSlave {
  val set_stb = Bool
  val set_addr = Bits(8 bits)
  val set_data = Bits(32 bits)
  def asMaster = {
    out(set_stb,set_addr,set_data)
  }  
}
trait hasReg {
  def ODWidth : Int
  def RegAddr : Int
  def at_reset : Int
  val bus = slave (new regBus)
  val setReg = setting_reg(RegAddr,8,ODWidth,at_reset)
  setReg.io.strobe  := bus.set_stb
  setReg.io.addr    := bus.set_addr
  setReg.io.i       := bus.set_data
  val buso          = setReg.io.o
  val buschanged    = setReg.io.changed

}
case class flow_control_responder(  WIDTH : Int = 64,
  SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  USE_TIME : Int = 0) extends Component with hasReg {
  val io = new Bundle {
    val force_fc_pkt = in Bool
    val clear = in Bool
    val fc = master Stream(axis(WIDTH))
    val i = slave Stream(axis(WIDTH))
    val o = master Stream(axis(WIDTH))
  }
  def RegAddr = nocShell.nocSRRegisters("FLOW_CTRL_BYTES_PER_ACK")
  def at_reset = 0
  def ODWidth = 32
  val enable_consumed = buso(31)
  val bytes_per_ack = buso(30 downto 0).asUInt
  val cvita = cVitaHdr(io.i.payload.data ## io.i.payload.data)
  val flow_control = Stream(axisu(64,128))
  val fc_valid = RegInit(False)
  val is_fc_ack = 
  (B(3 bits, (2 downto 1) -> cvita.pkt_type, 0 -> cvita.eob) === B(nocShell.pktType("FC_ACK"),3 bits))
  val is_data_pkt = 
  (B(3 bits, (2 downto 1) -> cvita.pkt_type, 0 -> False) === B(nocShell.pktType("DATA"),3 bits))
  val is_data_pkt_reg = RegInit(False)
  val pkt_count = Reg(UInt(16 bits)) init(0)
  val byte_count= Reg(UInt(32 bits)) init(0)
  val resp_pkt_count= Reg(UInt(16 bits)) init(0)
  val resp_byte_count= Reg(UInt(32 bits)) init(0)

  val fc_src_sid = Reg(Bits(16 bits)) init(0)
  val fc_dst_sid = Reg(Bits(16 bits)) init(0)
  val fc_vita_time = Reg(Bits(64 bits)) init(0)
  
  val fsm = new StateMachine {
    val ST_IDLE : State = new State with EntryPoint {
      whenIsActive {
        when(io.i.fire) {
          is_data_pkt_reg := is_data_pkt
          when(is_fc_ack || is_data_pkt) {
            fc_src_sid  := cvita.dst_sid;
            fc_dst_sid  := cvita.src_sid;
            byte_count  := byte_count + WIDTH/8
            when(io.i.payload.last) {
              goto(ST_IDLE)
            }.elsewhen(cvita.has_time) {
              goto(ST_TIME)
            }.otherwise {
              goto(ST_PAYLOAD)
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
              pkt_count  := io.i.payload.data(47 downto 32).asUInt
              byte_count := io.i.payload.data(31 downto 0).asUInt
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
    val dump = (isActive(ST_IDLE) && !is_data_pkt) || (!isActive(ST_IDLE) && !is_data_pkt_reg);
    io.o.valid := Mux(dump, False, io.i.valid)
    io.i.ready := Mux(dump, True , io.o.ready)
  }
  val byte_count_since_resp = byte_count - resp_byte_count
  when(byte_count_since_resp >= bytes_per_ack || io.force_fc_pkt) {
    fc_valid := enable_consumed
    resp_byte_count := byte_count
    resp_pkt_count := pkt_count
  }.elsewhen(flow_control.fire) {
    fc_valid := False
  }
  flow_control.valid := fc_valid
  val resp_cvita = new cVitaHdr
  
  io.o.payload.data  := io.i.payload.data
  io.o.payload.last  := io.i.payload.last
  flow_control.payload.data(63 downto 48) := B(0,16 bits)
  flow_control.payload.data(47 downto 32) := resp_pkt_count.asBits
  flow_control.payload.data(31 downto 0) := resp_byte_count.asBits
  flow_control.payload.last := True
  resp_cvita.pkt_type := B(nocShell.pktType("FC_RESP") >> 1, 2 bits)
  resp_cvita.eob := B(nocShell.pktType("FC_RESP") & 1, 1 bits)(0)
  resp_cvita.has_time := (if(USE_TIME == 0) False else True)
  resp_cvita.seqnum := B(0,12 bits)
  resp_cvita.src_sid := fc_src_sid
  resp_cvita.dst_sid := fc_dst_sid
  resp_cvita.length := B(0,16 bits)
  resp_cvita.vita_time := (if(USE_TIME == 0) B(0,64 bits) else fc_vita_time)
  flow_control.payload.user := resp_cvita.encode
  val chdr_framer_fc_pkt = chdr_framer(2,false)
  chdr_framer_fc_pkt.io.i << flow_control
  chdr_framer_fc_pkt.io.o >> io.fc
  chdr_framer_fc_pkt.io.clear := False  
}

case class packet_error_responder(  SR_ERROR_POLICY : Int = 1,
  USE_TIME : Int = 0) extends Component with hasReg{
  val io = new Bundle {
    val seqnum_error = out Bool
    val clear = in Bool
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val i = slave Stream(axis(64))
    val resp = master Stream(axis(64))
    val sid = in Bits(32 bits)
    val o = master Stream(axis(64))
  }
  def RegAddr = nocShell.nocSRRegisters("ERROR_POLICY")
  def ODWidth = 4
  def at_reset = 5
  val parser = cvita_hdr_parser(true)
  val policy_wait_until_next_burst = buso(3)
  val policy_wait_until_next_packet = buso(2)
  val policy_continue = buso(1)
  val send_error_pkt = buso(0)
  val clear_error = buschanged
  val hdr_stb = Bool
  val int = Stream(axis(64))
  val drop = Stream(axis(64))
  val cvita = new cVitaHdr
  parser.io.cvita <> cvita
  parser.io.i << io.i
  parser.io.o >> int 
  hdr_stb := parser.io.hdr_stb
  val seqnum_expected = Reg(UInt(12 bits)) init(0)
  val seqnum_hold = Reg(UInt(12 bits)) init(0)
  val seqnum_error = (seqnum_expected =/= cvita.seqnum.asUInt) && hdr_stb
  val error = Stream(axisu(64,128))
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
  drop << int.throwWhen(!(seqnum_error || error_hold) || policy_continue)
  io.o << drop
  val CODE_SEQ_ERROR = B(4,32 bits) ## B(0,4 bits) ## seqnum_expected ## 
  B(0,4 bits) ## seqnum_hold
  val CODE_SEQ_ERROR_MIDBURST = B(32,32 bits) ## B(0,4 bits) ## seqnum_expected ## 
  B(0,4 bits) ## seqnum_hold
  error.payload.data := (first_packet ? CODE_SEQ_ERROR | CODE_SEQ_ERROR_MIDBURST)
  error.payload.user := B"11" ## // 2
  (if(USE_TIME == 0) B"0" else B"1")  ## // 1
  B"1" ## B(0,12 bits) ## B(0,16 bits) ## // 1+12+16
  io.sid ## cvita.vita_time // 16
  error.payload.last := True
  val chdr_framer_resp_pkt = chdr_framer(2,false)
  chdr_framer_resp_pkt.io.i << error
  chdr_framer_resp_pkt.io.o >> io.resp
  chdr_framer_resp_pkt.io.clear := False  
  io.seqnum_error := seqnum_error
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
    val i = slave Stream(axis(64))
    val o = master Stream(axis(64))
    val cvita = master (new cVitaHdr)
  }
  val header = Reg(Bits(128 bits)) init 0
  val cvita = cVitaHdr(header)

  val firstTime = RegInit(True)
  val firstLine = RegInit(True)
  val readTime  = RegInit(False)
  val output = Stream(axis(64))
  
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
  USE_SEQ_NUM : Boolean = false) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val i = slave Stream(axisu(64,128))
    val o = master Stream(axis(64))
  }
  val length = Reg(UInt(16 bits)) init 0
  val seqnum = Reg(UInt(12 bits)) init 0
  
  val bodyi = Stream(axis(64))
  val bodyo = Stream(axis(64))
  val headeri = Stream(Bits(128 bits))
  val headero = Stream(Bits(128 bits))
  io.i.ready := headeri.ready && bodyi.ready
  headeri.valid := io.i.payload.last && io.i.valid && headeri.ready && bodyi.ready
  bodyi.payload.last := io.i.payload.last
  bodyi.valid := io.i.valid
  
  when(headeri.fire) {
    length := 8
  }.elsewhen(io.i.fire) {
    length := length + 8
  }
  
  headeri.payload := io.i.payload.user(127 downto 112) ## length ## io.i.payload.user(95 downto 0)
  headeri >-> headero
  bodyi.payload.data := io.i.payload.data
  val fifod = StreamFifo(dataType = axis(64),depth = SIZE)
  fifod.io.push << bodyi
  fifod.io.pop >> bodyo
  fifod.io.flush := io.clear

  val fsm = new StateMachine{
    val ST_IDLE : State = new State with EntryPoint {
      whenIsActive {
        when(headero.valid && bodyo.valid) {
          goto(ST_HEAD)
        }
      }
    }
    val ST_HEAD : State = new State {
      whenIsActive {
        when(io.o.ready) {
          when(headero.payload(125)) {
            goto(ST_TIME)
          } otherwise {
            goto(ST_BODY)
          }
        }
      }
    }
    val ST_TIME : State = new State {
      whenIsActive {
        when(io.o.ready) {
          goto(ST_BODY)
        }  
      }
    }
    val ST_BODY : State = new State {
      whenIsActive {
        when(io.o.ready && bodyo.payload.last) {
          goto(ST_IDLE)
        }
      }
    }
    val out_length = UInt(16 bits)
    when(headero.payload(125)) {
      out_length := headero.payload(111 downto 96).asUInt + 16 
    } otherwise {
      out_length := headero.payload(111 downto 96).asUInt + 8 
    }
    io.o.valid := isActive(ST_HEAD) || isActive(ST_TIME) || (bodyo.valid && isActive(ST_BODY))
    io.o.payload.last := isActive(ST_BODY) && bodyo.payload.last
    when(isActive(ST_HEAD)) {
      io.o.payload.data := headero.payload(127 downto 124) ## 
      (if(USE_SEQ_NUM) seqnum else headero.payload(123 downto 112)) ##
      out_length ## headero.payload(95 downto 64)
    }.elsewhen(isActive(ST_TIME)){
      io.o.payload.data := headero.payload(63 downto 0)
    }.otherwise {
      io.o.payload.data := bodyo.payload.data
    }
    bodyo.ready := isActive(ST_BODY) && io.o.ready
    headero.ready := (isActive(ST_TIME) || (isActive(ST_HEAD) && !headero.payload(125))) && io.o.ready
  }

  when(io.o.fire && io.o.payload.last) {
    seqnum := seqnum + 1     
  }
}

case class source_flow_control( WIDTH : Int = 64,
  SR_FLOW_CTRL_EN : Int = 0,
  SR_FLOW_CTRL_WINDOW_SIZE : Int = 1,
  SR_FLOW_CTRL_PKT_LIMIT : Int = 2) extends Component {
  val io = new Bundle {
    val i = slave Stream(axis(64))
    val clear = in Bool
    val fc = slave Stream(axis(64))
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val o = master Stream(axis(64))
    val reset = in Bool
    val busy = out Bool
  }
  
}
case class cmd_pkt_proc(  SR_AWIDTH : Int = 8,
  SR_DWIDTH : Int = 32,
  RB_AWIDTH : Int = 8,
  RB_USER_AWIDTH : Int = 8,
  RB_DWIDTH : Int = 64,
  USE_TIME : Int = 1,
  SR_RB_ADDR : Int = 0,
  SR_RB_ADDR_USER : Int = 1,
  FIFO_SIZE : Int = 5) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val set_stb = out Bool
    val set_addr = out Bits(SR_AWIDTH bits)
    val set_data = out Bits(SR_DWIDTH bits)
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
  
}
