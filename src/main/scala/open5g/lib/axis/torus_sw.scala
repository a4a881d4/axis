package open5g.lib.axis

import spinal.core._
import spinal.lib._

case class Torus_sw(config:TorusConfig) extends Component {

  val io = new Bundle {
    val mTerm = master(Axis(config.axisCfg))
    val sTerm = slave(Axis(config.axisCfg))
    val mX    = master(Axis(config.axisCfg))
    val sX    = slave(Axis(config.axisCfg))
    val mY    = master(Axis(config.axisCfg))
    val sY    = slave(Axis(config.axisCfg))

  }

}