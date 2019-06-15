package open5g.lib.zcpsm.plugins

import spinal.core._
import spinal.lib._
import open5g.lib.zcpsm._

trait peripheral {
  val AWidth      : Int
  val zBus = slave(zcpsmIORW(AWidth))
  val wBus = zBus.toWriteOnly()
}
trait hasEBus {
  def eBusFactory : Bundle
  val eBusName    : String
}

abstract class peripheralExt {
  def getName : String
  def applyIt(core : ZcpsmCore, port:Int) : Area
  var port : Int = -1
  def hasEBus : Boolean
}

trait PluginsExample {
  val code : String
  val config : zcpsmConfig
}
