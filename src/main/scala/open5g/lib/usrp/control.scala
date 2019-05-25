package open5g.lib.usrp

import spinal.core._
import spinal.lib._
import open5g.lib.axis.axis

case class synchronizer(  WIDTH             : Int = 1,
                          STAGES            : Int = 2,
                          INITIAL_VAL       : Int = 0,
                          FALSE_PATH_TO_IN  : Int = 1
                        ) extends Component {
  val io = new Bundle {
    val rst  = in Bool
    val sin  = in Bits(WIDTH bits)
    val sout = out Bits(WIDTH bits)
  }

  //Q: Why do we have a separate impl and instantiate
  //it with a different instance name based on this
  //arbitrary parameter FALSE_PATH_TO_IN?
  //A: To make constraining these synchronizers easier.
  //We would like to write a single false path constraint
  //for all synchronizers when the input is truly async.
  //However other cases might require constraining the input
  //of this module.
  //To enable this, all clients that hook up async signals to
  //the "in" port can set FALSE_PATH_TO_IN=1 (or use the default)
  //and all clients that want the "in" delay to be constrained can
  //set FALSE_PATH_TO_IN=0.
  //In the XDC we can write the following async constraint:
  //set_false_path -to [get_pins */synchronizer_false_path/stages[0].value_reg[0]/D]
  //and this will take care of all instances of this module with FALSE_PATH_TO_IN==1

  if(FALSE_PATH_TO_IN == 1) {
    val synchronizer_false_path = synchronizer_impl(
      WIDTH,STAGES,INITIAL_VAL)
    synchronizer_false_path.io <> io
  } else {
    val synchronizer_constrained = synchronizer_impl(
      WIDTH,STAGES,INITIAL_VAL)
    synchronizer_constrained.io <> io
  }

}

case class synchronizer_impl (  WIDTH  : Int,
                          STAGES : Int,
                          INITIAL_VAL : Int
                        ) extends Component {
  val io = new Bundle {
    val rst  = in Bool
    val sin  = in Bits(WIDTH bits)
    val sout = out Bits(WIDTH bits)
  }

  val value = Reg(Vec(Bits(WIDTH bits),STAGES))

  for(i <- 0 until STAGES) {
    when(io.rst) {
      value(i) := B(INITIAL_VAL,WIDTH bits)
    } otherwise {
      if(i == 0) {
        value(i) := io.sin
      } else {
        value(i) := value(i-1)
      }
    }
  }
  io.sout := value(STAGES-1)
}

case class por_gen() extends Component {
  val io = new Bundle {
    val reset_out = Bool
  }
  val por_counter = UInt(8 bits) init(0)
  when(por_counter =/= U"h55") {
    por_counter := por_counter + 1
    io.reset_out := True
  } otherwise {
    io.reset_out := False
  }
}

case class reset_sync() extends Component {
  val io = new Bundle {
    val reset_in = in Bool
    val reset_out = out Bool
  }
  val reset_double_sync = synchronizer(1,10,1,1)
  reset_double_sync.io.rst := False
  reset_double_sync.io.sin(0) := io.reset_in
  io.reset_out := reset_double_sync.io.sout(0) 
}

case class datapath_gatekeeper( WIDTH : Int = 64,
  COUNT_W : Int = 16) extends Component {
  val io = new Bundle {
    val flushing = out Bool
    val pkt_count = out Bits(COUNT_W bits)
    val s_axis = slave Stream(axis(WIDTH))
    val m_axis = master Stream(axis(WIDTH))
    val flush = in Bool
  }
  val monitor = axis_strm_monitor(COUNT_W = COUNT_W,
    PKT_LENGTH_EN = false, PKT_COUNT_EN = true, XFER_COUNT_EN = false)   
  monitor.io.axis_tlast  := io.s_axis.payload.last
  monitor.io.axis_tvalid := io.s_axis.valid
  monitor.io.axis_tready := io.s_axis.ready
  io.pkt_count := monitor.io.pkt_count
  val flusher = axis_packet_flush(WIDTH=WIDTH,FLUSH_PARTIAL_PKTS=0)
  flusher.io.enable := io.flush
  flusher.io.s_axis << io.s_axis
  flusher.io.m_axis >> io.m_axis
  io.flushing := flusher.io.flushing
}

case class axis_packet_flush( WIDTH : Int = 64,
  FLUSH_PARTIAL_PKTS : Int = 0) extends Component {
  val io = new Bundle {
    val enable = in Bool
    val flushing = out Bool
    val s_axis = slave Stream(axis(WIDTH))
    val m_axis = master Stream(axis(WIDTH))
  }
  val mid_pkt = RegInit(False)
  val active  = RegInit(False)

  when(io.s_axis.fire) {
    mid_pkt := !io.s_axis.payload.last
  }
  when(io.enable && 
    ((io.s_axis.fire && io.s_axis.payload.last) || (!mid_pkt && !io.s_axis.fire))
    ) {
    active := True
  }.elsewhen(!io.enable) {
    active := False
  }
  val flushing = (if(FLUSH_PARTIAL_PKTS == 0) active else io.enable)
  
  io.flushing := flushing
  io.m_axis.payload := io.s_axis.payload
  io.m_axis.valid := flushing ? False | io.s_axis.valid
  io.s_axis.ready := flushing ? True  | io.m_axis.ready  
}
case class axis_strm_monitor( COUNT_W : Int = 32,
  PKT_LENGTH_EN : Boolean = false,
  PKT_COUNT_EN : Boolean = false,
  XFER_COUNT_EN : Boolean = false) extends Component {
  val io = new Bundle {
    val sop = out Bool
    val xfer_count = out Bits(COUNT_W bits)
    val pkt_length = out Bits(16 bits)
    val pkt_count = out Bits(COUNT_W bits)
    val eop = out Bool
    val axis_tlast = in Bool
    val axis_tvalid = in Bool
    val axis_tready = in Bool
  }
  val pkt_head = RegInit(True)
  val xfer = io.axis_tvalid && io.axis_tready

  val sop = pkt_head && xfer
  val eop = xfer && io.axis_tlast
  io.eop := eop
  io.sop := sop
  when(pkt_head) {
    when(xfer) {
      pkt_head := !eop
    }.elsewhen(eop) {
      pkt_head := False
    }
  }

  if(PKT_LENGTH_EN) {
    val pkt_length = Reg(UInt(16 bits)) init 0
    when(eop) {
      pkt_length := 1
    }.elsewhen(xfer) {
      pkt_length := pkt_length + 1
    }
    io.pkt_length := pkt_length.asBits
  } else {
    io.pkt_length := B(0,16 bits)
  }

  if(PKT_COUNT_EN) {
    val pkt_count = Reg(UInt(COUNT_W bits)) init 0
    when(eop) {
      pkt_count := pkt_count + 1
    }
    io.pkt_count := pkt_count.asBits
  } else {
    io.pkt_count := B(0, COUNT_W bits)
  }

  if(XFER_COUNT_EN) {
    val xfer_count = Reg(UInt(COUNT_W bits)) init 0
    when(xfer) {
      xfer_count := xfer_count + 1
    }
    io.xfer_count := xfer_count.asBits
  } else {
    io.xfer_count := B(0, COUNT_W bits)
  }
}
