/***
  * 解决内存的动态使用问题



***/


package open5g.lib.virual.mem

import spinal.core._
import spinal.lib._

import open5g.lib.zcpsm._

case class vmConfig(phy:Int,vir:Int,page:Int,Bwidth:Int) {
  val blkAW = log2Up(page*8/Bwidth)
}
case class memBlockRecord(config:vmConfig) extends Bundle {
  val phyAddr = Bits(config.phy bits)
  val virAddr = Bits(config.vir bits)
}
class MemIn(config:vmConfig) extends Component {
  val io = new Bundle {
    val alloc =  slave(Stream(Bits(config.phy bits)))
    val done  = master(Stream(memBlockRecord(config)))
    val w     = master(zcpsmIOW(config.blkAW+config.phy,config.Bwidth))
    val error = out Bool
  }

}