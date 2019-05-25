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
  def pkt_type(pt:String) = B(pktType(pt) >> 1, 2 bits)
  def eob(pt:String) = Bool((pktType(pt)&1) == 1)
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
    val str_src = Vec(slave Stream(axis(64)),INPUT_PORTS)
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
  val clockBus = ClockDomain(io.bus_clk,io.bus_rst)
  val clockSys = ClockDomain(io.clk,io.reset)
  val ackin_2clk = StreamFifoCC(dataType = axis(64),
    depth = (1 << 5),
    pushClock = clockBus,
    popClock = clockSys)
  ackin_2clk.io.push << ackin_bclk
  ackin_2clk.io.pop >> io.ackin
  ackin_2clk.io.flush := False
}

case class noc_output_port( SR_FLOW_CTRL_EN : Int = 0,
  SR_FLOW_CTRL_WINDOW_SIZE : Int = 1,
  SR_FLOW_CTRL_PKT_LIMIT : Int = 2,
  PORT_NUM : Int = 0,
  MTU : Int = 10) extends Component {
  val wbus = slave(new regBus)
  val io = new Bundle {
    val str_src = slave Stream(axis(64))
    val clear = in Bool
    val dataout = master Stream(axis(64))
    val fcin = slave Stream(axis(64))
  }
  val fc = source_flow_control(
    SR_FLOW_CTRL_EN=SR_FLOW_CTRL_EN,
    SR_FLOW_CTRL_WINDOW_SIZE=SR_FLOW_CTRL_WINDOW_SIZE,
    SR_FLOW_CTRL_PKT_LIMIT=SR_FLOW_CTRL_PKT_LIMIT
    )
  fc.wbus <> wbus
  fc.io.clear := io.clear
  fc.io.fc << io.fcin
  fc.io.i << io.str_src
  fc.io.o >> io.dataout
}

case class noc_input_port(  SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  SR_ERROR_POLICY : Int = 2,
  STR_SINK_FIFOSIZE : Int = 11,
  USE_TIME : Boolean = false) extends Component {
  val wbus = slave(new regBus)
  val io = new Bundle {
    val clear = in Bool
    val fc = master Stream(axis(64))
    val i = slave Stream(axis(64))
    val resp_sid = in Bits(32 bits)
    val o = master Stream(axis(64))
  }
  
  val int,fc,resp = Stream(axis(64))
  val axi_fifo_receive_window = chdr_fifo_large(SIZE=STR_SINK_FIFOSIZE)
  axi_fifo_receive_window.io.clear := io.clear
  axi_fifo_receive_window.io.i << io.i
  axi_fifo_receive_window.io.o >> int
  val responder = noc_responder(SR_FLOW_CTRL_BYTES_PER_ACK=SR_FLOW_CTRL_BYTES_PER_ACK,
    SR_ERROR_POLICY=SR_ERROR_POLICY,
    USE_TIME=USE_TIME)
  responder.io.clear := io.clear
  responder.wbus <> wbus
  responder.io.i << int
  responder.io.o >> io.o
  responder.io.fc >> fc
  responder.io.resp >> resp
  responder.io.resp_sid := io.resp_sid
  val mux = axi_mux(PRIO = 0,
    WIDTH = 64,
    PRE_FIFO_SIZE = 0,
    POST_FIFO_SIZE = 1,
    SIZE = 2)
  mux.io.clear := io.clear
  mux.io.i(0) << fc
  mux.io.i(1) << resp
  mux.io.o >> io.fc
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
  USE_TIME : Boolean = false) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val resp_sid = in Bits(32 bits)
    val fc = master Stream(axis(64))
    val i = slave Stream(axis(64))
    val resp = master Stream(axis(64))
    val o = master Stream(axis(64))
  }
  val wbus = slave(new regBus)
  val respFC = flow_control_responder(SR_FLOW_CTRL_BYTES_PER_ACK=SR_FLOW_CTRL_BYTES_PER_ACK,USE_TIME=USE_TIME)
  val respPE = packet_error_responder(SR_ERROR_POLICY=SR_ERROR_POLICY,USE_TIME=USE_TIME)
  wbus <> respPE.bus
  wbus <> respFC.bus
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
  setReg.wbus <> bus
  val buso          = setReg.io.o
  val buschanged    = setReg.io.changed

}
case class flow_control_responder(  WIDTH : Int = 64,
  SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  USE_TIME : Boolean = false) extends Component with hasReg {
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
  resp_cvita.has_time := (if(USE_TIME) True else False)
  resp_cvita.seqnum := B(0,12 bits)
  resp_cvita.src_sid := fc_src_sid
  resp_cvita.dst_sid := fc_dst_sid
  resp_cvita.length := B(0,16 bits)
  resp_cvita.vita_time := (if(USE_TIME) fc_vita_time else B(0,64 bits))
  flow_control.payload.user := resp_cvita.encode
  val chdr_framer_fc_pkt = chdr_framer(2,false)
  chdr_framer_fc_pkt.io.i << flow_control
  chdr_framer_fc_pkt.io.o >> io.fc
  chdr_framer_fc_pkt.io.clear := False  
}

case class packet_error_responder(  SR_ERROR_POLICY : Int = 1,
  USE_TIME : Boolean = false) extends Component with hasReg{
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
  (if(USE_TIME) B"1" else B"0")  ## // 1
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
    val changed = out Bool
    val o = out Bits(width bits)
  }
  val wbus = slave(new regBus)

  val output = Reg(Bits(width bits)) init(at_reset)
  val changed = RegInit(False)
  when(wbus.set_stb && wbus.set_addr === my_addr) {
    output := wbus.set_data(width-1 downto 0)
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
    val o = master Stream(axis(64))
    val busy = out Bool
  }
  val wbus = slave (new regBus)
  val sr_enable = setting_reg(nocShell.nocSRRegisters("FLOW_CTRL_EN"),8,4,0)
  val window_reset = sr_enable.io.changed
  val sfc_enable = sr_enable.io.o(0)
  val window_enable = sr_enable.io.o(1)
  val pkt_limit_enable = sr_enable.io.o(2)
  val fc_ack_disable = sr_enable.io.o(3)
  val sr_window_size = setting_reg(nocShell.nocSRRegisters("FLOW_CTRL_WINDOW_SIZE"),8,32,0)
  val window_size = sr_window_size.io.o.asUInt
  val sr_pkt_limit = setting_reg(nocShell.nocSRRegisters("FLOW_CTRL_PKT_LIMIT"),8,16,0)
  val pkt_limit = sr_pkt_limit.io.o.asUInt

  val last_pkt_consumed = Reg(UInt(16 bits))
  val last_byte_consumed = Reg(UInt(32 bits))
  val window_reseting = RegInit(True)
  val window_reset_cnt = Reg(UInt(12 bits))
  
  val fc_ack_cnt = Reg(UInt(6 bits)) init 0

  when(io.clear || window_reset) {
    window_reseting  := True
    window_reset_cnt := 0
  }.elsewhen(window_reseting && !io.i.valid) {
    window_reset_cnt := window_reset_cnt + 1
    window_reset := (window_reset_cnt === U"12'hFFF")
  }
  val cvita_hdr_parser_fc = cvita_hdr_parser(false)
  cvita_hdr_parser_fc.io.i.payload := io.fc.payload
  cvita_hdr_parser_fc.io.i.valid   := io.fc.valid
  io.fc.ready := cvita_hdr_parser_fc.io.o.ready  
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
  pkt_cnt_end       := last_pkt_consumed + pkt_limit
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
        pkt_len_reg := (pkt_len & B"1111_1111_1111_1000").asUInt
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
    }
    is(ST_FC_ACK_HEAD) {
      go := (!window_enable || (window_free_space >= 16)) && (!pkt_limit_enable || (pkts_free_space >= 1))
      when(io.o.fire) {
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
    io.o.payload := io.i.payload
    io.o.valid := False
    io.i.ready := True
  } otherwise {
    switch(state) {
      is(ST_IDLE) {
        io.o.payload := io.i.payload
        io.o.valid := False
        io.i.ready := False
      }
      is(ST_FC_ACK_HEAD) {
        io.o.payload.data  := ack.encode(127 downto 64)
        io.o.payload.last  := False
        io.o.valid := go
        io.i.ready  := False
      }
      is(ST_FC_ACK_PAYLOAD) {
        io.o.payload.data  := current_pkt_cnt ## current_byte
        io.o.payload.last  := True
        io.o.valid := True
        io.i.ready  := False
      }
      is(ST_DATA_PKT) {
        io.o.payload := io.i.payload
        io.o.valid   := io.i.valid && go
        io.i.ready   := io.o.ready && go
      }
    }
    default {
      io.o.payload := io.i.payload
      io.o.valid   := io.i.valid
      io.i.ready   := io.o.ready
    }
  }
  io.busy     := window_reseting
  io.fc.ready := True // FC RESP is a non-flow controlled path, so always accept packets
}
case class cmd_pkt_proc(  SR_AWIDTH : Int = 8,
  SR_DWIDTH : Int = 32,
  RB_AWIDTH : Int = 8,
  RB_USER_AWIDTH : Int = 8,
  RB_DWIDTH : Int = 64,
  USE_TIME : Boolean = true,
  SR_RB_ADDR : Int = 0,
  SR_RB_ADDR_USER : Int = 1,
  FIFO_SIZE : Int = 5) extends Component {
  val wbus = master(new regBus)
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
  val cmdfifo = StreamFifo(dataType=axis(64),depth=FIFO_SIZE)
  val cmd = Stream(axis(64))
  val int = Stream(axis(64))
  cmdfifo.io.push << io.cmd
  cmdfifo.io.pop >> cmd
  cmdfifo.io.flush := io.clear

  val pkt_vita_time_hold = Reg(Bits(64 bits))
  val has_time_hold = RegInit(False)
  val seqnum_hold = Reg(Bits(12 bits))
  val src_sid_hold, dst_sid_hold = Reg(Bits(16 bits))
  val int_tready = RegInit(False)

  val hdr_parser = cvita_hdr_parser(false) 
  val parser = hdr_parser.cvita
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
  val set_rb_addr      = int.payload.data(SR_AWIDTH-1+32 downto 32) === B(SR_RB_ADDR,SR_AWIDTH bits)
  val set_rb_addr_user = int.payload.data(SR_AWIDTH-1+32 downto 32) === B(SR_RB_ADDR_USER,SR_AWIDTH bits)
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
    io.resp.valid       := False
    wbus.set_stb        := False
    io.set_has_time     := False
    io.rb_addr          := B(0)
    io.rb_addr_user     := B(0)
  } otherwise {
    switch(state) {
      is(S_CMD_HEAD) {
        io.resp.valid := False
        io.resp.payload.last := False
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
          io.set_time     := pkt_vita_time_hold
          io.set_has_time := has_time_hold
          when(set_rb_addr) {
            io.rb_addr       := int.payload.data(RB_AWIDTH-1 downto 0)
          }.elsewhen(set_rb_addr_user) {
            io.rb_addr_user  := int.payload.data(RB_USER_AWIDTH-1 downto 0)
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
          io.resp.payload.last  := False
          io.resp.payload.data  := resp_header
          io.resp.valid         := True
          state                 := S_RESP_HEAD
        }
      }
      is(S_RESP_HEAD) {
        when(io.resp.fire) {
          io.resp.valid := True
          if(USE_TIME) {
            io.resp.payload.last := False
            io.resp.payload.data := resp_time
            state := S_RESP_TIME
          } else {
            io.resp.payload.last := True
            io.resp.payload.data := rb_data_hold
            state := S_RESP_DATA
          }
        }
      }
      is(S_RESP_TIME) {
        when(io.resp.fire) {
          io.resp.payload.last  := True
          io.resp.payload.data  := rb_data_hold
          io.resp.valid := True
          state       := S_RESP_DATA
        }
      }
      is(S_RESP_DATA) {
        when(io.resp.fire) {
          io.resp.payload.last  := False
          io.resp.valid := False
          state       := S_CMD_HEAD
        }
      }
      is(S_DROP) {
        when(int.valid && int.payload.last) {
          state       := S_CMD_HEAD
        }
      }
      default {
        state       := S_CMD_HEAD
      }
    }
  }
  
}
