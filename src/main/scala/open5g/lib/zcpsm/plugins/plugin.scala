package open5g.lib.zcpsm.plugins

import spinal.core._
import spinal.lib._
import scala.collection._
import open5g.lib.debug.{Debugable,dbBundle}
import open5g.lib.zcpsm._

case class zcpsmConfig(PWidth:Int,HWidth:Int,psm:String) {
  val AWidth = 8-HWidth
  val ext = mutable.Map[Int,peripheralExt]()
  def addperipheral(p:Int,a:peripheralExt) = ext += (p -> a)
  def all = {
    val exts = List(
      new zcpsmExt(AWidth,"GP0"),
      new zcpsmExt(AWidth,"GP1"),
      new zcpsmMemSmall(0,AWidth,64,"ParaMem"),
      new zcpsmMemBig(8,AWidth,16,"NocMem"),
      new zcpsmMemIn(0,AWidth, 64, "Ingress"),
      new zcpsmMemOut(0,AWidth, 64, "Egress0"),
      new zcpsmMemRegOut(8,AWidth, 16, "Egress1"),
      new zcpsmBusExt(2,2,AWidth,"DebugIO"),
      new zcpsmAxisMaster(1,AWidth,"AxisOut"),
      new zcpsmAxisSlave(1,AWidth,"AxisIn"),
      new zcpsmStreamMaster(1,AWidth,"StreamOut"),
      new zcpsmStreamSlave(1,AWidth,"StreamIn")
    )
    for(i <- 0 until exts.length) addperipheral(i,exts(i))
  }
  import open5g.lib.zcpsm.tools.asmParser
  val parser = new asmParser
  val (program,labeled) = parser.fromFile(psm)
  def initProg = for(i <- 0 until (1<<PWidth)) yield {
    if(i < program.length) B(BigInt(program(i).toHex),18 bits) else B(0,18 bits)
  }
}
import spinal.core.internals.Misc
case class ZcpsmCore(cfg:zcpsmConfig,
  debug: Boolean = false) extends Component with Debugable{
  val io = new Bundle {
    val prog = slave(zcpsmIOW(cfg.PWidth,18))
  }
  val dbIn = Bits(db.inAlloc bits)

  val cpu = zcpsm(cfg.PWidth,debug)
  val progMem = Mem(Bits(18 bits),initialContent = cfg.initProg)
  progMem.write( data    = io.prog.out_port,
                 address = io.prog.port_id.asUInt,
                 enable  = io.prog.write_strobe)
  cpu.io.prog.instruction := progMem.readSync(cpu.io.prog.address)
  val dList = cfg.ext.keys.toList.sorted
  val dec = zcpsmDecode(8,cfg.HWidth,dList)
  dec.io.busM <> cpu.io.iobus
  val portMap = dList.zipWithIndex.toMap
  val plugs = for((p,a) <- cfg.ext) yield (p -> a.applyIt(this,portMap(p)))
  
  def eBus(port:Int) = {
    var r:Bundle = null
    val p = plugs(port)
    Misc.reflect(p,(n,o) => {
      o match {
          case o:Bundle => if(n == "eBus") {
            r=o
          }
          case _ =>
        }
      })
    r
  }
  val dbPort = db finalDb
}

abstract class zcpsmExample(example:PluginsExample) 
    extends Component with Debugable {
    val core = ZcpsmCore(example.config,debug)
    val dbIn = Bits(db.inAlloc bits)
    core.io.prog.write_strobe := False
    core.io.prog.out_port := B(0,18 bits)
    core.io.prog.port_id := B(0,example.config.PWidth bits)
    val dbPort = db finalDb
  }
