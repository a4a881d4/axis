package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import open5g.lib.axis.{axis,axisu}
import open5g.lib.common._

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
  def RBA(rb:String) = nocRBRegisters(rb) 
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
  def pkt_type(pt:String) = B(pktType(pt), 3 bits)
  def pkt_part(pt:String) = B(pktType(pt) >> 1, 2 bits)
  def eob(pt:String) = Bool((pktType(pt)&1) == 1)
}

case class noc_shell(  NOC_ID : BigInt,
  INPUT_PORTS : Int = 1,
  OUTPUT_PORTS : Int = 1,
  USE_TIMED_CMDS : Boolean = false,
  STR_SINK_FIFOSIZE : List[Int] = List.fill(1)(11),
  MTU : List[Int] = List.fill(1)(10),
  USE_GATE_MASK : Int = 0,
  CMD_FIFO_SIZE : List[Int] = List.fill(1)(5),
  RESP_FIFO_SIZE : Int = 5) extends Component {
  val BLOCK_PORTS = if(INPUT_PORTS > OUTPUT_PORTS) INPUT_PORTS else OUTPUT_PORTS
  val BWIDTH = log2Up(BLOCK_PORTS)
  val IWDITH = log2Up(INPUT_PORTS)
  val OWDITH = log2Up(OUTPUT_PORTS)
  val io = new Bundle {
    val bus = slave (new CLK)
    val sys = slave (new CLK)
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
    val ackin = master Stream(axis(64))
    val o = master Stream(axis(64))
    val vita_time = in Bits(64 bits)
  }
  val NOC_SHELL_MAJOR_COMPAT_NUM = 5
  val NOC_SHELL_MINOR_COMPAT_NUM = 1
  val RB_AWIDTH = 3
  val clockBus = io.bus.clkDomain
  val clockSys = io.sys.clkDomain
  val cmdin,cmdin_bclk,cmdout,cmdout_bclk = Stream(axis(64))
  val ackin,ackin_bclk,ackout,ackout_bclk = Stream(axis(64))
  io.cmdout >> cmdout
  val ackin_2clk, cmdin_2clk = StreamFifoCC(dataType = axis(64),
    depth = (1 << 5), pushClock = clockBus, popClock = clockSys)
  ackin >> io.ackin
  ackin_2clk.io.push << ackin_bclk
  ackin_2clk.io.pop >> ackin
  cmdin_2clk.io.push << cmdin_bclk
  cmdin_2clk.io.pop >> cmdin
  val ackout_2clk, cmdout_2clk = StreamFifoCC(dataType = axis(64),
    depth = (1 << 5), pushClock = clockSys, popClock = clockBus)
  ackout_2clk.io.push << ackout
  ackout_2clk.io.pop >> ackout_bclk
  cmdout_2clk.io.push << cmdout
  cmdout_2clk.io.pop >> cmdout_bclk
  val src_sid,src_sid_bclk = Vec(Bits(16 bits),BLOCK_PORTS)
  val resp_in_dst_sid,resp_in_dst_sid_bclk = Vec(Bits(16 bits),INPUT_PORTS)
  val next_dst_sid,resp_out_dst_sid = Vec(Bits(16 bits),OUTPUT_PORTS)
  
  io.next_dst_sid := next_dst_sid
  io.resp_out_dst_sid := resp_out_dst_sid
  io.resp_in_dst_sid := resp_in_dst_sid
  io.src_sid := src_sid

  io.debug := B(0,64 bits)

  val set_stb = Bits(BLOCK_PORTS bits)
  val set_data = Vec(Bits(32 bits),BLOCK_PORTS)
  val set_addr = Vec(Bits(8 bits),BLOCK_PORTS)
  io.set_stb := set_stb
  io.set_addr := set_addr
  io.set_data := set_data
  val sid_settings_2clk = StreamFifoCC(dataType = Vec(Bits(16 bits),BLOCK_PORTS),
    depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
  sid_settings_2clk.io.push.payload := src_sid
  sid_settings_2clk.io.push.valid := True
  sid_settings_2clk.io.pop.ready := True
  src_sid_bclk := sid_settings_2clk.io.pop.payload
  val resp_settings_2clk = StreamFifoCC(dataType = Vec(Bits(16 bits),INPUT_PORTS),
    depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
  resp_settings_2clk.io.push.payload := resp_in_dst_sid
  resp_settings_2clk.io.push.valid := True
  resp_settings_2clk.io.pop.ready := True
  resp_in_dst_sid_bclk := resp_settings_2clk.io.pop.payload

  val flush_datain, flush_dataout, flush_datain_bclk, flush_dataout_bclk = Bool
  val datain_pkt_cnt, dataout_pkt_cnt, datain_pkt_cnt_bclk, dataout_pkt_cnt_bclk = Bits(16 bits)
  val fcout,dataout_post,dataout = Stream(axis(64))
  val datain_pre,fcin,datain = Stream(axis(64))
  val wbus_bclk, wbus = Vec(new regBus,BLOCK_PORTS)
  val clear_rx_stb_bclk, clear_rx_flush, clear_rx_clear, clear_rx_trig = Bits(INPUT_PORTS bits)
  val clear_tx_stb_bclk, clear_tx_flush, clear_tx_clear, clear_tx_trig = Bits(OUTPUT_PORTS bits)
  val clear_rx_stb = Bits(INPUT_PORTS bits) 
  val clear_tx_stb = Bits(INPUT_PORTS bits) 
  val wbus_2clk = StreamFifoCC(dataType = Vec(new regBus,BLOCK_PORTS), 
    depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
  wbus_2clk.io.push.payload := wbus
  wbus_2clk.io.push.valid := True
  wbus_2clk.io.pop.ready := True
  wbus_bclk := wbus_2clk.io.pop.payload
  
  val BusClockArea = new ClockingArea(clockBus) {
    val output_mux = axi_mux(PRIO = 0, WIDTH = 64, PRE_FIFO_SIZE = 0, POST_FIFO_SIZE = 2, SIZE = 4)
    output_mux.io.clear := False
    output_mux.io.i(0) << dataout_post
    output_mux.io.i(1) << fcout
    output_mux.io.i(2) << cmdout_bclk
    output_mux.io.i(3) << ackout_bclk
    output_mux.io.o >> io.o
    val input_demux = axi_demux(WIDTH = 64, SIZE = 4, PRE_FIFO_SIZE = 2, POST_FIFO_SIZE = 0)
    val vheader = input_demux.io.header
    val pkt_type_in = vheader(63 downto 62) ## vheader(60)
    val vdest = Reg(UInt(2 bits)) init 0
    switch(pkt_type_in) {
      is(nocShell.pkt_type("DATA"))     {vdest := 0}
      is(nocShell.pkt_type("DATA_EOB")) {vdest := 0}
      is(nocShell.pkt_type("FC_RESP"))  {vdest := 1}
      is(nocShell.pkt_type("FC_ACK"))   {vdest := 0}
      is(nocShell.pkt_type("CMD"))      {vdest := 2}
      is(nocShell.pkt_type("CMD_EOB"))  {vdest := 2}
      is(nocShell.pkt_type("RESP"))     {vdest := 3}
      is(nocShell.pkt_type("RESP_ERR")) {vdest := 3}
      // default                           {vdest := 0}
    }
    input_demux.io.clear := False
    input_demux.io.dest  := vdest.asBits
    input_demux.io.i << io.i
    input_demux.io.o(0) >> datain_pre
    input_demux.io.o(1) >> fcin
    input_demux.io.o(2) >> cmdin_bclk
    input_demux.io.o(3) >> ackin_bclk
    val keeper_in,keeper_out = datapath_gatekeeper(WIDTH=64,COUNT_W=16)
    keeper_in.io.s_axis << datain_pre
    keeper_in.io.m_axis >> datain
    keeper_in.io.flush := flush_datain_bclk
    datain_pkt_cnt_bclk := keeper_in.io.pkt_count

    keeper_out.io.s_axis << dataout
    keeper_out.io.m_axis >> dataout_post
    keeper_out.io.flush := flush_dataout_bclk
    dataout_pkt_cnt_bclk := keeper_out.io.pkt_count
    
    if(OUTPUT_PORTS > 1) {
      val odmux = axi_mux(PRIO = 0, WIDTH = 64, PRE_FIFO_SIZE=0, POST_FIFO_SIZE=0, SIZE = OUTPUT_PORTS)
      odmux.io.clear := False
      val fcdemux = axi_demux(WIDTH = 64, PRE_FIFO_SIZE=0, POST_FIFO_SIZE=0, SIZE = OUTPUT_PORTS)
      fcdemux.io.clear := False
      for (i <- 0 until OUTPUT_PORTS) {
        val noc_outp = noc_output_port()
        noc_outp.io.clear := clear_tx_stb_bclk(i)
        noc_outp.wbus <> wbus_bclk(i)
        noc_outp.io.dataout >> odmux.io.i(i)
        noc_outp.io.fcin << fcdemux.io.o(i)
        noc_outp.io.str_src << io.str_src(i)

      }
      odmux.io.o >> dataout
      fcdemux.io.i << fcin
      val header_fcin = fcdemux.io.header
      fcdemux.io.dest := header_fcin(OWDITH-1 downto 0)
    } else {
      val noc_outp = noc_output_port()
      noc_outp.io.clear := clear_tx_stb_bclk(0)
      noc_outp.wbus <> wbus_bclk(0)
      noc_outp.io.dataout >> dataout
      noc_outp.io.fcin << fcin
      noc_outp.io.str_src << io.str_src(0)
    }
    if(INPUT_PORTS == 1) {
      val noc_inp = noc_input_port(STR_SINK_FIFOSIZE=STR_SINK_FIFOSIZE(0))
      noc_inp.io.clear := clear_rx_stb_bclk(0)
      noc_inp.io.resp_sid := src_sid_bclk(0) ## resp_in_dst_sid_bclk(0)
      noc_inp.wbus <> wbus_bclk(0)
      noc_inp.io.i << datain
      noc_inp.io.o >> io.str_sink(0)
      noc_inp.io.fc >> fcout
    } else {
      val din = axi_demux(WIDTH = 64, SIZE=INPUT_PORTS)
      din.io.clear := False
      val header_datain = din.io.header
      din.io.dest := header_datain(IWDITH-1 downto 0)
      din.io.i << datain
      val fcmux = axi_mux(WIDTH=64,SIZE=INPUT_PORTS)
      fcmux.io.clear := False
      fcmux.io.o >> fcout
      for(i <- 0 until INPUT_PORTS) {
        val noc_inp = noc_input_port(STR_SINK_FIFOSIZE = STR_SINK_FIFOSIZE(i))
        noc_inp.io.clear := clear_rx_stb_bclk(i)
        noc_inp.io.resp_sid := src_sid_bclk(i) ## resp_in_dst_sid_bclk(i)
        noc_inp.wbus <> wbus_bclk(i)
        noc_inp.io.i << din.io.o(i)
        noc_inp.io.o >> io.str_sink(i)
        noc_inp.io.fc >> fcmux.io.i(i)  
      }  
    }
  }

  val data_cnt_2clk = StreamFifoCC(dataType = Bits(32 bits), 
    depth = (1 << 5), pushClock = clockBus, popClock = clockSys)
  val data_cnt_bclk = Stream(Bits(32 bits))
  data_cnt_bclk.valid := True
  data_cnt_bclk.payload := datain_pkt_cnt_bclk ## dataout_pkt_cnt_bclk
  val data_cnt = Stream(Bits(32 bits))
  data_cnt.ready  := True
  datain_pkt_cnt  := data_cnt.payload(31 downto 16)
  dataout_pkt_cnt := data_cnt.payload(15 downto 0)
  data_cnt_2clk.io.push << data_cnt_bclk
  data_cnt_2clk.io.pop >> data_cnt


  flush_datain := clear_rx_flush.orR
  flush_dataout := clear_tx_flush.orR

  val cmdin_ports,ackout_ports = Vec(Stream(axis(64)),BLOCK_PORTS)
  val cmd_header = Bits(64 bits)

  
  val SysClockArea = new ClockingArea(clockSys) {
    val clear_rx_stb_int = Reg(Bits(INPUT_PORTS bits)) 
    val clear_tx_stb_int = Reg(Bits(INPUT_PORTS bits))

    clear_rx_stb := clear_rx_stb_int
    clear_tx_stb := clear_tx_stb_int

    val demux = axi_demux(WIDTH = 64, PRE_FIFO_SIZE = 0, POST_FIFO_SIZE = 0, SIZE = BLOCK_PORTS)
    demux.io.clear := False
    cmd_header := demux.io.header
    demux.io.dest := cmd_header(BWIDTH-1 downto 0)
    demux.io.i << cmdin
    val ackout_mux = axi_mux(PRIO = 0, WIDTH = 64, PRE_FIFO_SIZE = RESP_FIFO_SIZE, POST_FIFO_SIZE = 0, SIZE = BLOCK_PORTS) 
    ackout_mux.io.clear := False
    ackout_mux.io.o >> ackout
    for(i <- 0 until BLOCK_PORTS) {
      demux.io.o(i) >> cmdin_ports(i)
      val rb_stb_int = RegInit(False)
      val rb_data_int = Reg(Bits(64 bits))
      val rb_addr_noc_shell = Bits(RB_AWIDTH bits)
      val cmd_proc = cmd_pkt_proc(SR_AWIDTH=8,SR_DWIDTH=32,RB_AWIDTH=RB_AWIDTH,
         RB_USER_AWIDTH = 8, RB_DWIDTH = 64, USE_TIME = USE_TIMED_CMDS,
         FIFO_SIZE = CMD_FIFO_SIZE(i))
      cmd_proc.io.cmd << cmdin_ports(i)
      cmd_proc.io.resp >> ackout_ports(i)
      cmd_proc.io.vita_time := io.vita_time
      ackout_mux.io.i(i) << ackout_ports(i)
      io.set_has_time(i) := cmd_proc.io.set_has_time
      cmd_proc.wbus <> wbus(i)
      set_stb(i) := wbus(i).set_stb
      set_addr(i) := wbus(i).set_addr
      set_data(i) := wbus(i).set_data
      cmd_proc.io.clear := False
      cmd_proc.io.rb_stb := rb_stb_int
      cmd_proc.io.rb_data := rb_data_int
      rb_addr_noc_shell := cmd_proc.io.rb_addr
      io.rb_addr(i) := cmd_proc.io.rb_addr_user
      io.set_time(i) := cmd_proc.io.set_time
      switch(rb_addr_noc_shell.asUInt) {
        is(nocShell.RBA("NOC_ID")) {
          rb_stb_int := True
          rb_data_int := B(NOC_ID,64 bits)
        }
        is(nocShell.RBA("GLOBAL_PARAMS")) {
          rb_stb_int := True
          rb_data_int := datain_pkt_cnt ## dataout_pkt_cnt ## B"16'd0" ## B(INPUT_PORTS,8 bits) ## B(OUTPUT_PORTS,8 bits)
        }
        is(nocShell.RBA("FIFOSIZE")) {
          rb_stb_int := True
          rb_data_int := (if(i < INPUT_PORTS) B(STR_SINK_FIFOSIZE(i),64 bits) else B(0,64 bits))
        }
        is(nocShell.RBA("MTU")){
          rb_stb_int := True
          rb_data_int := (if(i < OUTPUT_PORTS) B(MTU(i),64 bits) else B(0,64 bits))
        }
        is(nocShell.RBA("BLOCK_PORT_SIDS")){
          rb_stb_int := True
          rb_data_int := src_sid(i) ## (if(i < OUTPUT_PORTS) next_dst_sid(i) else B(0,16 bits)) ##
          (if(i < INPUT_PORTS) resp_in_dst_sid(i) else B(0,16 bits)) ##
          (if(i < OUTPUT_PORTS) resp_out_dst_sid(i) else B(0,16 bits))
        }
        is(nocShell.RBA("USER_DATA")){
          rb_stb_int := io.rb_stb(i)
          rb_data_int := io.rb_data(i)
        }
        is(nocShell.RBA("NOC_SHELL_COMPAT_NUM")){
          rb_stb_int := True
          rb_data_int := B(NOC_SHELL_MAJOR_COMPAT_NUM,32 bits) ## 
          B(NOC_SHELL_MINOR_COMPAT_NUM, 32 bits)
        }
        default {
          rb_stb_int := True
          rb_data_int := B"64'h0BADC0DE0BADC0DE"
        }
      }
      when(set_stb(i)) {
        rb_stb_int := False
      }
      val sr_block_sid = setting_reg(my_addr = nocShell.nocSRRegisters("SRC_SID"), width=16, at_reset=0)
      sr_block_sid.wbus := wbus(i)
      src_sid(i) := sr_block_sid.io.o 
      if(i < INPUT_PORTS) {
        val sr_clear_rx_fc = setting_reg(my_addr = nocShell.nocSRRegisters("CLEAR_RX_FC"), width = 2, at_reset = 0)
        sr_clear_rx_fc.wbus <> wbus(i)
        clear_rx_clear(i) := sr_clear_rx_fc.io.o(0)
        clear_rx_flush(i) := sr_clear_rx_fc.io.o(1)
        clear_rx_trig(i) := sr_clear_rx_fc.io.changed
        clear_rx_stb_int(i) := clear_rx_clear(i) && clear_rx_trig(i)
        val sr_resp_in_dst_sid = setting_reg(my_addr = nocShell.nocSRRegisters("RESP_IN_DST_SID"), width = 16, at_reset = 0)
        sr_resp_in_dst_sid.wbus <> wbus(i)
        resp_in_dst_sid(i) := sr_resp_in_dst_sid.io.o
      }
      if(i < OUTPUT_PORTS) {
        val sr_clear_tx_fc = setting_reg(my_addr = nocShell.nocSRRegisters("CLEAR_TX_FC"), 
          width = 2, at_reset = 0)
        sr_clear_tx_fc.wbus <> wbus(i)
        clear_tx_clear(i) := sr_clear_tx_fc.io.o(0)
        clear_tx_flush(i) := sr_clear_tx_fc.io.o(1)
        clear_tx_trig(i) := sr_clear_tx_fc.io.changed
        clear_tx_stb_int(i) := clear_tx_clear(i) & clear_tx_trig(i)
        val sr_next_dst_sid = setting_reg(my_addr = nocShell.nocSRRegisters("NEXT_DST_SID"), 
          width = 16, at_reset = 0)
        sr_next_dst_sid.wbus <> wbus(i)
        next_dst_sid(i) := sr_next_dst_sid.io.o
        val sr_resp_out_dst_sid = setting_reg(my_addr = nocShell.nocSRRegisters("RESP_OUT_DST_SID"), 
          width = 16, at_reset = 0)
        sr_resp_out_dst_sid.wbus <> wbus(i)
        resp_out_dst_sid(i) := sr_resp_out_dst_sid.io.o
        io.clear_tx_seqnum(i) := clear_tx_stb(i)
      }  
    }
  }
  for(i <- 0 until INPUT_PORTS) {
    val clear_rx_stb_sync = StreamFifoCC(dataType = Bool, depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
    clear_rx_stb_sync.io.push.payload := clear_rx_stb(i)
    clear_rx_stb_sync.io.push.valid := True
    clear_rx_stb_sync.io.pop.ready := True
    clear_rx_stb_bclk(i) := clear_rx_stb_sync.io.pop.payload
  }
  for(i <- 0 until OUTPUT_PORTS) {
    val clear_tx_stb_sync = StreamFifoCC(dataType = Bool, depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
    clear_tx_stb_sync.io.push.payload := clear_tx_stb(i)
    clear_tx_stb_sync.io.push.valid := True
    clear_tx_stb_sync.io.pop.ready := True
    clear_tx_stb_bclk(i) := clear_tx_stb_sync.io.pop.payload
  }
  val flush_sync_out = StreamFifoCC(dataType = Bool, depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
  flush_sync_out.io.push.payload := flush_dataout
  flush_sync_out.io.push.valid := True
  flush_sync_out.io.pop.ready := True
  flush_dataout_bclk := flush_sync_out.io.pop.payload

  val flush_sync_in = StreamFifoCC(dataType = Bool, depth = (1 << 1), pushClock = clockSys, popClock = clockBus)
  flush_sync_in.io.push.payload := flush_datain
  flush_sync_in.io.push.valid := True
  flush_sync_in.io.pop.ready := True
  flush_datain_bclk := flush_sync_in.io.pop.payload

}

case class noc_output_port() extends Component {
  val wbus = slave(new regBus)
  val io = new Bundle {
    val str_src = slave Stream(axis(64))
    val clear = in Bool
    val dataout = master Stream(axis(64))
    val fcin = slave Stream(axis(64))
  }
  val fc = source_flow_control()
  fc.wbus <> wbus
  fc.io.clear := io.clear
  fc.io.fc << io.fcin
  fc.io.i << io.str_src
  fc.io.o >> io.dataout
}

case class noc_input_port(STR_SINK_FIFOSIZE : Int = 11,
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
  val responder = noc_responder(USE_TIME=USE_TIME)
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
    POST_FIFO_SIZE = 2,
    SIZE = 2)
  mux.io.clear := io.clear
  mux.io.i(0) << fc
  mux.io.i(1) << resp
  mux.io.o >> io.fc
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

case class noc_responder(USE_TIME : Boolean = false) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val resp_sid = in Bits(32 bits)
    val fc = master Stream(axis(64))
    val i = slave Stream(axis(64))
    val resp = master Stream(axis(64))
    val o = master Stream(axis(64))
  }
  val wbus = slave(new regBus)
  val respFC = flow_control_responder(USE_TIME=USE_TIME)
  val respPE = packet_error_responder(USE_TIME=USE_TIME)
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

case class packet_error_responder(USE_TIME : Boolean = false) extends Component with hasReg{
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

