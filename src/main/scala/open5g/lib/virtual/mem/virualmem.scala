/***
  * 内存的动态管理
  * 内存不需要在管理器内
  * 假设输入为axis流以及起始地址
  * 地址和page对齐，初步考虑page size = 256 byte
  * 输出
  * 
  *
  *   指定内存区间对应的page，stream(page address)
  *
  *   ------------------------------------
  *   |Tag index|High Address|Low Address|
  *   | 8 bits  |   6 bits   |  8 bits   |
  *   ------------------------------------
  *        |
  *      4 bits + High Address -> TLB(10 bits)
  *
  * 
  *   Unit                -> 空闲page
  *   add(page -> adress) -> Unit
  *   release(adress)     -> Unit 
  *   get(address + len)  -> stream(page address)
  *



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