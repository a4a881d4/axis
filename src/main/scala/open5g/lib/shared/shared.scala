package open5g.lib.shared

import spinal.core._
import spinal.lib._

class shared(BW: Int, SW: Int) 
  extends Bundle with IMasterSlave {
  val f    = Bool
  val b    = Bits(BW bits)
  val s    = Bits(SW bits)

  override def asMaster(): Unit = {
    out(f,b,s)
  }
  
  def brocast[T <: Data](bType : HardType[T]) = {
    val r = bType()
    r.assignFrom(b)
    r
  } 
  
  def share[T <: Data](sType : HardType[T]) = {
    val r = sType()
    r.assignFrom(s)
    r
  } 
}

case class sharedConfig( BW   : Int,
                         SW   : Int,
                         Size : Int
) {
  def W = log2Up(Size)
  def safe = 256
}

trait sharedItem {
  val config : sharedConfig
  val bi =  slave(shared(config.BW,config.SW))
  val bo = master(shared(config.BW,config.SW))
  def connect(p:Int):Unit = {
    Misc.reflect(this,(n,o) => o match {
      case ep:sharedEP => {
        ep.bi <> bi
        ep.bo <> bo
        ep.pos := U(p)
      }
      case ri:sharedItem => {
        ri.bi <> bi
        ri.bo <> bo
        ri.connect(p)
      }
    })
  }
}

class allocMem(config: sharedConfig,isResp:Bool) extends Bundle with IMasterSlave {
  val ch    = UInt(config.W bits)
  val addr  = UInt(config.SW bits)
  val alloc = if(isResp) UInt(config.SW bits) else null 
  val len   = UInt(8 bits)
  def asMaster = {
    out(ch,addr,len)
    if(isResp) out(alloc)
  }
}

abstract class sharedEP(val config: sharedConfig) extends Component with sharedItem{
  val pos   = in UInt(config.W bits)
  val count = RegInit(U(0,config.W bits))
  val io = new Bundle {
    val state = in Bits(config.BW bits)
    val req   =  slave(Stream(allocMem(config,false)))
    val resp  = master(Stream(allocMem(config,true)))
  }
  when(pos === 0) {
    when(count === config.Size-1) {
      count := 0
    } otherwise {
      count := count + 1
    }
  }.elsewhen(bi.f) {
    count := 0
  } otherwise {
    count := count + 1
  }
  when(current === 0) {
    bo.b := state
  } otherwise {
    bo.b := bi.b
  }
  def current = Mux(pos >= count, pos - count, config.Size + pos - count)
  def safe = (bi.b(config.SW-1 downto 0).asUInt + (~bi.s).asUInt + 1) > config.safe
  def done = safe & req(current)
  val req = Mem(Bool,config.Size)
  req.write(address = io.reqCh,data = io.req,enable = io.req | done)
  when(io.get & io.getCh < config.Size)) {
    when(resp(io.getCh)) {
      resp(io.getCh) := False
    }
  }.elsewhen(done) {
    resp(current) := True
  }
}