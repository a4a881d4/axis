package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import open5g.lib.axis.{axis,axisu}

// Experimental debugging block for collecting packet statistics
// such as throughput, packet size, and flow control congestion.
//

case class noc_block_debug( NOC_ID : Int = 0xDEB1200000000000,
  STR_SINK_FIFOSIZE : Int = 11) extends Component {
  val io = new Bundle {
    val bus = slave clock()
    val ce = slave clock()
    val i = slave Stream(axis(64))
    val debug = out Bits(64 bits)
    val o = master Stream(axis(64))
  }
  
  val INPUT_PORTS = 1
  val OUTPUT_PORTS = 2
  val BLOCK_PORTS = if(INPUT_PORTS > OUTPUT_PORTS) INPUT_PORTS else OUTPUT_PORTS
  
  val shell = noc_shell(NOC_ID = NOC_ID, 
    INPUT_PORTS = INPUT_PORTS, 
    OUTPUT_PORTS = OUTPUT_PORTS, 
    STR_SINK_FIFOSIZE = STR_SINK_FIFOSIZE)
  io.i >> shell.io.i
  io.o << shell.io.o
  io.bus <> shell.bus
  io.ce <> shell.io.sys
  io.debug := shell.io.debug
  val set_data = shell.io.set_data
  val set_addr = shell.io.set_addr
  val set_stb = shell.io.set_stb
  val rb_addr = shell.io.rb_addr
  val clear_tx_seqnum = shell.io.clear_tx_seqnum
  
  val rb_data = Reg(Vec(Bits(64 bits),BLOCK_PORTS))
  shell.io.rb_data := rb_data
  shell.io.rb_stb := B"11"
  
  val cmdout,ackin = Stream(axis(64))
  cmdout >> shell.io.cmdout
  ackin << shell.io.ackin
  
  val str_sink = Vec(Stream(axis(64)),INPUT_PORTS)
  (0 until INPUT_PORTS).foreach{i => 
    str_sink(i) << shell.io.str_sink(i)
  }
  
  val str_src = Vec(Stream(axis(64)),OUTPUT_PORTS)
  (0 until OUTPUT_PORTS).foreach{i =>
    str_src(i) >> shell.io.str_src
  }

  val src_sid = shell.io.src_sid
  val next_dst_sid = shell.io.next_dst_sid 

  cmdout.payload.data := B(0,64 bits)
}
