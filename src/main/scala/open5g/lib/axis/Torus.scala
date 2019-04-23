package open5g.lib.axis

import spinal.core._
import spinal.lib._

case class TorusConfig( axisCfg         : AxisConfig,
                        DIM_SIZE        : Int,
                        TERM_BUFF_SIZE  : Int,
                        XB_BUFF_SIZE    : Int,
                        ROUTING_ALLOC   : String,
                        SWITCH_ALLOC    : String
                      )

