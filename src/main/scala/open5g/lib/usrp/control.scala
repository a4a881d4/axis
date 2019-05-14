package open5g.lib.usrp

import spinal.core._
import spinal.lib._

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

