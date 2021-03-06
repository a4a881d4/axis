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
case class peripheralMatchBig( matchGroup:Int,
                            matchDepth:Int,
                            BW        :Int,
                            MW        :Int,
                            AWidth    :Int,
                            Depth     :Int,
                            eBusName  :String
  ) extends Component with peripheral with hasEBus {
    def width    = log2Up(Depth)
    val haddr    = RegInit(U(0,width-MW bits))
    val laddr    = RegInit(U(0,MW bits))
    val haddrInt = Mux(zBus.written(1),zBus.out_port.asUInt(width-1 downto MW),haddr)
    val inc      = (laddr.andR === True) | zBus.written(4)
    when(zBus.read(0)) {
      laddr := laddr + 1
      when(inc) {
        haddr := haddr + 1
      }
    } otherwise {
      when(zBus.written(1)) {
        haddr := zBus.out_port.asUInt(width-1 downto MW) + 1
        laddr := zBus.out_port.asUInt(MW-1 downto 0)
      }
    }
  
    def ramA = if(BW>0) {
      val block = wBus.Q(2)(BW-1 downto 0).asUInt
      block @@ haddrInt
    } else haddrInt

    val mixWidth = 1<<MW
    val mAWD = log2Up(matchDepth)
    val mAWG = log2Up(matchGroup)
    val mAW = mAWD + mAWG + 1
    def eBusFactory = slave(new Match.meBus(width+BW-MW,8*mixWidth,AWidth))
    val eBus = eBusFactory
    val pMem = Mem(Bits(8*mixWidth bits), (Depth/mixWidth)*(1<<BW))
    val read = pMem.readSync(address = ramA, enable = (zBus.written(1) | inc)).subdivideIn(8 bits)
    pMem.write(
            address = eBus.data.port_id.asUInt,
            data    = eBus.data.out_port,
            enable  = eBus.data.write_strobe)  
    val data  = read(laddr)
    val mMem  = List.fill(matchGroup*2)(Mem(Bits(8 bits),matchDepth))
    val select = eBus.mdata.Q(2).asUInt(mAWG downto 0)
    val mra   = RegInit(U(0,mAWD bits))
    val sum   = RegInit(B(0,matchGroup bits))
    val mradd = Mux(zBus.written(3),zBus.out_port.asUInt(mAWD-1 downto 0),mra)
    val mren  = zBus.written(3) | zBus.read(0)
    val mdata = Vec(Bits(8 bits),matchGroup)
    val mask  = Vec(Bits(8 bits),matchGroup)
    when(zBus.read(0)) {
      mra := mra + 1
    } otherwise {
      when(zBus.written(3)) {
        mra := zBus.out_port.asUInt(mAWD-1 downto 0) + 1
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
    for(i <- 0 until matchGroup) {
      mdata(i) :=   mMem(2*i).readSync( address = mradd, enable = mren)
      mask(i)  := mMem(2*i+1).readSync( address = mradd, enable = mren)
    }
    when(zBus.read(0)) {
      for(i <- 0 until matchGroup) {
        sum(i) := sum(i) | 
          ((data ^ mdata(i)) & mask(i)).orR
      }
    } otherwise {
      when(zBus.written(3)) {
        sum := B(0, matchGroup bits)
      }
    }
    zBus.in_port := Mux(zBus.ce & zBus.read_strobe, 
      Mux(zBus.port_id(0),(B(0, 8-matchGroup bits) ## ~sum),data),
      B(0,8 bits))
}
class zcpsmMatchBig( matchGroup:Int,
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
    val eb = peripheralMatchBig(matchGroup,matchDepth,BW,MW,AWidth,Depth,eBusName)
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
      |OUTPUT s00, 01
      |OUTPUT s00, 22
      |LOAD   s00, 00
      |OUTPUT s00, 23
      |OUTPUT s00, 21
      |LOAD   s00, s00 ;; Nop
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
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
    config.addperipheral(2,new zcpsmMatchBig(1,16,4,3,config.AWidth,64,"Match"))
    config.addperipheral(3,new zcpsmStreamSlave(1,config.AWidth,"StreamIn"))
  }
    object TestTwoGroup extends PluginsExample {
    val code = """
      |;; 0x normal 1x match data 2x run 3x stream
      |;; 11 base 12 group data/mask 10 write data 
      |;; 20 match 23 match base 21 data base 22 block
      |CALL   Init
      |L0:
      |CALL   READ_IO
      |OUTPUT s00, 01
      |OUTPUT s00, 22
      |LOAD   s00, 00
      |OUTPUT s00, 23
      |LOAD   s00, 06
      |OUTPUT s00, 21
      |LOAD   s00, s00 ;; Nop
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
      |INPUT  s01, 20
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
      |;; group1
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
      |;; group2
      |;; mac  = 0xffffffffffff
      |;; mask = 0xffffffffffff
      |LOAD   s01, 02
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
      |LOAD   s01, 03
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
    val config = zcpsmConfig(11,4,code)
    config.addperipheral(0,new zcpsmExt(config.AWidth,"GP0"))
    config.addperipheral(1,new zcpsmExt(config.AWidth,"MP"))
    config.addperipheral(2,new zcpsmMatchBig(8,16,6,3,config.AWidth,64,"Match"))
    config.addperipheral(3,new zcpsmStreamSlave(1,config.AWidth,"StreamIn"))
  }
  class zcpsmMatchForTest(example:PluginsExample,val debug:Boolean = false) 
    extends zcpsmExample(example) {
    val io = new Bundle {
      val bus  = master(zcpsmIORW(example.config.AWidth))
      val data = slave(zcpsmIOW(9,64))
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