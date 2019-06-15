package open5g.lib.zcpsm.plugins

import open5g.lib.zcpsm._
import open5g.lib.zcpsm.plugins._
import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import open5g.lib.debug.{Debugable,dbBundle,DebugUtils}
import open5g.lib.zcpsm.tools._
object SimUtils {
  def iowDump(name:String,bus:zcpsmIORW) = {
    val write_strobe = bus.write_strobe.toBoolean
    val out_port     = bus.out_port.toInt
    val port_id      = bus.port_id.toInt
    val ce           = bus.ce.toBoolean
    if(ce && write_strobe) println(name,f"${port_id}%02x",f"${out_port}%02x")
  }
  def debugDump(dut:zcpsmExample) = {
    val dbitem = if(dut.debug) dut.db.DebugItem else null
    if(dut.debug) {
      val cap = DebugUtils.Capture2Signal(dut.dbPort.capture.toBigInt,dbitem)
      val ins = cap("/core/cpu/ins").intValue
      val pc = cap("/core/cpu/pc").intValue
      val instruction = cap("/core/cpu/instruction").intValue
      val asm         = binIns(ins).disAsm

      println("debug",
        f"$ins%05x",
        f"$pc%05x",
        f"$instruction%05x",
        asm.toString
      )
    }
  }
}