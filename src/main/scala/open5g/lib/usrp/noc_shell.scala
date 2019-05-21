package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import open5g.lib.axis.axis

object NoCSRRegisters extends SpinalEnum {
  val FLOW_CTRL_BYTES_PER_ACK     = newElement() // 1;
  val FLOW_CTRL_WINDOW_SIZE       = newElement() // 2;
  val FLOW_CTRL_EN                = newElement() // 3;
  val ERROR_POLICY                = newElement() // 4;
  val SRC_SID                     = newElement() // 5;
  val NEXT_DST_SID                = newElement() // 6;
  val RESP_IN_DST_SID             = newElement() // 7;
  val RESP_OUT_DST_SID            = newElement() // 8;
  val FLOW_CTRL_PKT_LIMIT         = newElement() // 9;
  val RB_ADDR_USER                = newElement() // 124;
  val CLEAR_RX_FC                 = newElement() // 125;
  val CLEAR_TX_FC                 = newElement() // 126;
  val RB_ADDR                     = newElement() // 127;
  // Registers 128-255 for users
  val USER_REG_BASE               = newElement() // 128;

  val mapping = Map(
    FLOW_CTRL_BYTES_PER_ACK     -> 1,
    FLOW_CTRL_WINDOW_SIZE       -> 2,
    FLOW_CTRL_EN                -> 3,
    ERROR_POLICY                -> 4,
    SRC_SID                     -> 5,
    NEXT_DST_SID                -> 6,
    RESP_IN_DST_SID             -> 7,
    RESP_OUT_DST_SID            -> 8,
    FLOW_CTRL_PKT_LIMIT         -> 9,
    RB_ADDR_USER                -> 124,
    CLEAR_RX_FC                 -> 125,
    CLEAR_TX_FC                 -> 126,
    RB_ADDR                     -> 127,
    // Registers 128-255 for users
    USER_REG_BASE               -> 128
  )
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    FLOW_CTRL_BYTES_PER_ACK     -> 1,
    FLOW_CTRL_WINDOW_SIZE       -> 2,
    FLOW_CTRL_EN                -> 3,
    ERROR_POLICY                -> 4,
    SRC_SID                     -> 5,
    NEXT_DST_SID                -> 6,
    RESP_IN_DST_SID             -> 7,
    RESP_OUT_DST_SID            -> 8,
    FLOW_CTRL_PKT_LIMIT         -> 9,
    RB_ADDR_USER                -> 124,
    CLEAR_RX_FC                 -> 125,
    CLEAR_TX_FC                 -> 126,
    RB_ADDR                     -> 127,
    // Registers 128-255 for users
    USER_REG_BASE               -> 128)
}

object NoCRBRegisters extends SpinalEnum(defaultEncoding=binarySequential) {
  val NOC_ID,GLOBAL_PARAMS,FIFOSIZE,MTU,BLOCK_PORT_SIDS,USER_DATA,NOC_SHELL_COMPAT_NUM = newElement() 
}

object NoCRegisters extends SpinalEnum(defaultEncoding=binarySequential) {
  val DATA_PKT,DATA_EOB_PKT,FC_RESP_PKT,FC_ACK_PKT,CMD_PKT,CMD_EOB_PKT,RESP_PKT,RESP_ERR_PKT = newElement() 
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

case class flow_control_responder(  WIDTH : Int = 64,
  SR_FLOW_CTRL_BYTES_PER_ACK : Int = 1,
  USE_TIME : Int = 0) extends Component {
  val io = new Bundle {
    val force_fc_pkt = in Bool
    val clk = in Bool
    val clear = in Bool
    val fc = master Stream(axis(WIDTH,-1))
    val set_stb = in Bool
    val set_addr = in Bits(8 bits)
    val set_data = in Bits(32 bits)
    val i = slave Stream(axis(WIDTH,-1))
    val reset = in Bool
    val o = master Stream(axis(WIDTH,-1))
  }
}

case class packet_error_responder(  SR_ERROR_POLICY : Int = 1,
  USE_TIME : Int = 0) extends Component {
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
  val parser = cvita_hdr_parser(true)
  val setReg = setting_reg(4,4,5)
    //NoCSRRegisters.mapping(ERROR_POLICY),4,5)
  setReg.io.strobe := io.set_stb
  setReg.io.addr := io.set_addr
  setReg.io.i := io.set_data
  val policy_wait_until_next_burst = setReg.io.o(3)
  val policy_wait_until_next_packet = setReg.io.o(2)
  val policy_continue = setReg.io.o(1)
  val send_error_pkt = setReg.io.o(0)
  val clear_error = setReg.io.changed
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