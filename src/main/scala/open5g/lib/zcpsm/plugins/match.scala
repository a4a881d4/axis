package open5g.lib.zcpsm.plugins

import spinal.core._
import spinal.lib._
import open5g.lib.zcpsm._

object Match {
  class meBus(val da:Int,val dd:Int, val AWidth:Int) extends Bundle with IMasterSlave {
    val data = zcpsmIOW(da,dd)
    val mdata = zcpsmIOW(AWidth)
    def asMaster = {
      data.asMaster
      mdata.asMaster
    }
  }
}
case class peripheralMatch( matchGroup:Int,
                            matchDepth:Int,
                            BW        :Int,
                            MW        :Int,
                            AWidth    :Int,
                            Depth     :Int,
                            eBusName  :String
  ) extends Component with zcpsmMemBlocked with hasEBus {
    val mixWidth = 1<<MW
    val mAWD = log2Up(matchDepth)
    val mAWG = log2Up(matchGroup)
    val mAW = mAWD + mAWG + 1
    def eBusFactory = slave(new Match.meBus(width+BW-MW,8*mixWidth,AWidth))
    val eBus = eBusFactory
    val pMem = Mem(Vec(Bits(8 bits),mixWidth), (Depth/mixWidth)*(1<<BW))
    val read = pMem.readSync(ramA(width+BW-1 downto MW))
    pMem.write(
            address = eBus.data.port_id.asUInt,
            data    = eBus.data.out_port.subdivideIn(8 bits),
            enable  = eBus.data.write_strobe)  
    val data = read(ramA(MW-1 downto 0))
    val mMem = List.fill(matchGroup*2)(Mem(Bits(8 bits),matchDepth))
    val select = eBus.mdata.Q(2).asUInt(mAWG downto 0)
    val mra = RegInit(U(0,mAWD bits))
    val sum = RegInit(B(0,matchGroup bits))
    when(zBus.read(0)) {
      mra := mra + 1
    } otherwise {
      when(zBus.written(3)) {
        mra := zBus.out_port.asUInt(mAWD-1 downto 0)
      }
    }
    val mwa = RegInit(U(0,mAWD bits))
    when(eBus.mdata.written(0)) {
      mwa := mwa + 1
    } otherwise {
      when(eBus.mdata.written(1)) {
        mwa := eBus.mdata.out_port.asUInt(mAWD-1 downto 0)
      }
    }
    for(i <-0 until matchGroup*2) {
      mMem(i).write(
          address = mwa,
          data    = eBus.mdata.out_port,
          enable  = eBus.mdata.written(0) && (select === i) // write  
      )
    }
    when(zBus.read(0)) {
      for(i <- 0 until matchGroup) {
        sum(i) := sum(i) | 
          ((data ^ mMem(2*i).readSync(mra)) & mMem(2*i+1).readSync(mra)).orR
      }
    } otherwise {
      when(zBus.written(3)) {
        sum := B(0, matchGroup bits)
      }
    }
    zBus.in_port := Mux(zBus.ce & zBus.read_strobe, 
      Mux(zBus.port_id(0),data,B(0, 8-matchGroup bits) ## sum),
      B(0,8 bits))
}
class zcpsmMatch( matchGroup:Int,
                  matchDepth:Int,
                  BW        :Int,
                  MW        :Int,
                  AWidth    :Int,
                  Depth     :Int,
                  eBusName  :String
  ) extends peripheralExt {
  def getName = "zcpsmMatch"
  def hasEBus = true
  def applyIt(core : ZcpsmCore, decport:Int) = new Area {
    import core._
    val eb = peripheralMatch(matchGroup,matchDepth,BW,MW,AWidth,Depth,eBusName)
    val eBus = eb.eBusFactory
    eBus <> eb.eBus
    eBus.setName(eb.eBusName)
    dec.io.busS(decport) <> eb.zBus 
    port = dList(decport) 
  }
}
object ExampleMatch {
  object TestOneGroup extends PluginsExample {
    val code = """
      |;; 0x normal 1x match data 2x run 3x stream
      |;; 11 base 12 group data/mask 10 write data 
      |;; 20 match 23 match base 21 data base 22 block
      |CALL   Init
      |L0:
      |CALL   READ_IO
      |OUTPUT s00, 22
      |LOAD   s00, 00
      |OUTPUT s00, 21
      |OUTPUT s00, 23
      |LOAD   s02, 05
      |LOAD   s00, 00
      |Loop6:
      |INPUT  s01, 20
      |SUBCY  s02, 01
      |JUMP   NC, Loop6
      |INPUT  s00, 21
      |OUTPUT s00, 00
      |JUMP   L0
      |;; data in s00 ;; s01 temp reg
      |READ_IO:      
      |Wait_valid:
      |INPUT  s01, 31
      |AND    s01, 01
      |JUMP   Z, Wait_valid
      |INPUT  s00, 30
      |RETURN
      |Init:
      |;; mac  = 0x0123456789ab
      |;; mask = 0xffffffffffff
      |LOAD   s01, 00
      |OUTPUT s01, 11
      |OUTPUT s01, 12
      |LOAD   s01, 01
      |OUTPUT s01, 10
      |LOAD   s01, 23
      |OUTPUT s01, 10
      |LOAD   s01, 45
      |OUTPUT s01, 10
      |LOAD   s01, 67
      |OUTPUT s01, 10
      |LOAD   s01, 89
      |OUTPUT s01, 10
      |LOAD   s01, AB
      |OUTPUT s01, 10
      |LOAD   s01, 01
      |OUTPUT s01, 12
      |LOAD   s01, 00
      |OUTPUT s01, 11
      |LOAD   s01, FF
      |OUTPUT s01, 10
      |OUTPUT s01, 10
      |OUTPUT s01, 10
      |OUTPUT s01, 10
      |OUTPUT s01, 10
      |OUTPUT s01, 10
      |RETURN      
      """.stripMargin
    val config = zcpsmConfig(6,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmExt(config.AWidth,"MP"))
    config.addperipheral(2,new zcpsmMatch(1,16,4,3,config.AWidth,64,"Match"))
    config.addperipheral(3,new zcpsmStreamSlave(1,config.AWidth,"StreamIn"))
  }
  class zcpsmMatchForTest(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus  = master(zcpsmIORW(example.config.AWidth))
      val data = slave(zcpsmIOW(7,64))
      val sin  = slave(Stream(Bits(8 bits)))
    }
    io.bus     <> core.eBus(0).asInstanceOf[zcpsmIORW]
    val mbus   =  core.eBus(2).asInstanceOf[Match.meBus]
    val mdata  =  core.eBus(1).asInstanceOf[zcpsmIORW]
    mbus.data  <> io.data
    mbus.mdata <> mdata.toWriteOnly
    mdata.in_port := B(0,8 bits)
    io.sin >> core.eBus(3).asInstanceOf[Stream[Bits]]
  }
}