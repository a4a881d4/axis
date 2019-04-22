package open5g.lib.axis

import spinal.core._
import spinal.lib._



/**
 * Configuration class for the Axi4 Stream bus
 */
case class AxisConfig(dataWidth    : Int,
                      idWidth      : Int = -1,
                      useId        : Boolean = false) {

  def arwUserWidth = Math.max(arUserWidth, awUserWidth)

  if(useId)
    require(idWidth >= 0,"You need to set idWidth")

  def dataType = Bits(dataWidth bits)
  def idType = UInt(idWidth bits)
  def bytePerWord = dataWidth/8
}

trait AxisBus

case class Axis(config:AxisConfig) extends Bundle with IMasterSlave with AxisBus {

  val m = Stream(AxisM(config))
  val s = Stream(AxisS(config))
  def masterCmd  = m
  def slaveCmd   = s

  def asMaster() {
    master(m)
    slave(s)
  }

  def <<(that : Axis) : Unit = that >> this
  def >> (that : Axis) : Unit = {
    this.masterCmd drive that.masterCmd
    that.slaveCmd drive this.slaveCmd
  }
}

class AxisM(val config:AxisConfig) extends Bundle {
  val tdata  = config.dataType
  val tvalid = Bool
  val tlast  = Bool
  val id     = if(config.useId)     config.idType   else null
}

class AxisS(val config:AxisConfig) extends Bundle {
  val tready = Bool
}
