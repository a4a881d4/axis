package open5g.lib.axis

import spinal.core._
import spinal.lib._



/**
 * Configuration class for the Axi4 Stream bus
 */
case class AxisConfig(dataWidth : Int) 

trait AxisBus

case class Axis(config:AxisConfig,userWidth:Int) extends Bundle with IMasterSlave with AxisBus {

  val m = Stream(new AxisM(config,userWidth))
  val s = Stream(new AxisS(config))
  def masterCmd  = m
  def slaveCmd   = s

  def asMaster() {
    master(m)
    slave(s)
  }

  def << (that : Axis) : Unit = that >> this
  def >> (that : Axis) : Unit = {
    this.masterCmd drive that.masterCmd
    that.slaveCmd drive this.slaveCmd
  }
}

class AxisM(val config:AxisConfig,val userWidth:Int) extends Bundle {
  val tdata  = Bits(config.dataWidth bits)
  val tvalid = Bool
  val tlast  = Bool
  val tuser  = if(userWidth >= 0)   Bits(userWidth bits)        else null
}

class AxisS(val config:AxisConfig) extends Bundle {
  val tready = Bool
}

object AxisPriv{

  def driveWeak[T <: Data](source : Bundle,sink : Bundle, by : T,to : T,defaultValue : () => T,allowResize : Boolean,allowDrop : Boolean) : Unit = {
    (to != null,by != null) match {
      case (false,false) =>
      case (true,false) => if(defaultValue != null) to := defaultValue() else LocatedPendingError(s"$source can't drive $to because this first doesn't has the corresponding pin")
      case (false,true) => if(!allowDrop) LocatedPendingError(s"$by can't drive $sink because this last one doesn't has the corresponding pin")
      case (true,true) => to := (if(allowResize) by.resized else by)
    }
  }

  def driveAxM[T <: AxisM](stream: Stream[T],sink: Stream[T]): Unit = {
    driveWeak(stream,sink,stream.tuser,sink.tuser,() => B(sink.tuser.range -> false),true,true)
  }

}


object AxisM{
  def apply(config: AxisConfig,userWidth:Int) = new AxisM(config,userWidth)

  implicit class StreamPimper(stream : Stream[AxisM]) {
    def drive(sink: Stream[AxisM]): Unit = {
      sink.arbitrationFrom(stream)
      AxisPriv.driveAxM(stream,sink)
      sink.tvalid := stream.tvalid
      sink.tlast  := stream.tlast
      sink.tdata  := stream.tdata 
    }
  }
}

object AxisS {
  def apply(config:AxisConfig) = new AxisS(config)
  implicit class StreamPimper(stream : Stream[AxisS]) {
    def drive(sink: Stream[AxisS]): Unit = {
      sink.arbitrationFrom(stream)
      sink.tready  := stream.tready 
    }
  }
}
