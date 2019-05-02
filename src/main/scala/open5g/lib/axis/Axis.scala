package open5g.lib.axis

import spinal.core._
import spinal.lib._



/**
 * Configuration class for the Axi4 Stream bus
 */
case class AxisConfig(dataWidth : Int) 

case class Axis(config:AxisConfig,userWidth:Int) extends Bundle with IMasterSlave {

  val tdata  = Bits(config.dataWidth bits)
  val tvalid = Bool
  val tlast  = Bool
  val tuser  = if(userWidth >= 0)   Bits(userWidth bits)        else null
  val tready = Bool

  def asMaster() {
    out(tdata,tvalid,tlast)
    if(userWidth>=0) out(tuser)
    in(tready)
  }

  def << (that : Axis) : Unit = that >> this
  def >> (that : Axis) : Unit = {
    this drive that
    this.tready := that.tready
  }

  def ddrive(that : Axis) : Unit = {
    that.tdata  := this.tdata
    that.tlast  := this.tlast
    if(this.userWidth == that.userWidth) that.tuser := this.tuser
  }

  def drive(that : Axis) : Unit = {
    this ddrive that
    that.tvalid := this.tvalid
  }

  def ExtBits = if(userWidth>=0) tlast.asBits ## tuser ## tdata else tlast.asBits ## tdata
  def FromBits(b:Bits) : Unit = {
    tlast := b.msb
    if(userWidth>0) tuser := b(userWidth+config.dataWidth-1 downto config.dataWidth)
    tdata := b(config.dataWidth-1 downto 0)
  }
  def ExtLen = if(userWidth>=0) config.dataWidth+userWidth+1 else config.dataWidth+1 
}
