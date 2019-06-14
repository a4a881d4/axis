package open5g.lib.debug

import spinal.core._
import spinal.lib._
import spinal.core.internals._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class dbBundle(aIn:Int,aOut:Int) extends Bundle {
  val inject  = if(aIn > 0)  in  Bits(aIn  bits) else null
  val capture = if(aOut > 0) out Bits(aOut bits) else null
}

trait debugitem
case class injection(name:String,active:Int,pos:Int,width:Int) extends debugitem
case class captured(name:String,pos:Int,width:Int) extends debugitem
    
trait Debugable {
  val debug: Boolean
  val io: Bundle
  val dbIn: Bits
  val dbPort: dbBundle
  def DataInCom = DataInObj(this)
  def DataInIo  = DataInObj(io).map{case (o,n) => o -> ("io_"+n)}
  def DataInObj(root:Object): Map[Data,String] = {
    var datas = Map[Data,String]()
    def findInB(b:Bundle,name:String):Unit = {
      datas += (b -> name)
      b.elements.foreach{ case (n,o) => o match {
        case d:Bundle => findInB(d,name+"_"+n)
        case d:Data   => datas += (d -> (name+"_"+n))
        case _ =>
        }}
    }
    def findInSub(com:Component,parentName:String) = {
      Misc.reflect(com, (name,obj) => {
        obj match {
          case d:Bundle => findInB(d,parentName+"/"+name)
          case _ =>
        }
      })
    }
    def travel(o: Object):Unit = {
      Misc.reflect(o,(name, obj) => {
        obj match {
          case data: Bundle => findInB(data,name)
          case data: Data => datas += (data -> name)
          case com: Component => findInSub(com,name)
          case _ => 
        }
      })
    }
    travel(root)
    datas
  }
  object db {
    var InBuf  = ArrayBuffer[(Data,(Int,Int))]()
    var SignalIn  = ArrayBuffer[(String,(Int,Int,Option[Debugable]))]()
    var inAlloc  = 0
    var OutBuf = ArrayBuffer[(Data,(Int,Int))]()
    var SignalOut = ArrayBuffer[(String,(Int,Int,Option[Debugable]))]()
    var outAlloc = 0
    var IoOutBuf = ArrayBuffer[(Data,(Int,Int))]()
    var SignalIoOut = ArrayBuffer[(String,(Int,Int,Option[Debugable]))]()
    var ioOutAlloc = 0
    
    def obj2name(data:Data):Option[String] = DataInCom.get(data)
    def in[T <: Data](dataType: HardType[T]) = {
      val r = cloneOf(dataType)
      r.asInput
      if(debug) {
        val dl = r.getBitsWidth
        InBuf += (r->(dl,inAlloc))
        inAlloc += dl+1
      }
      r
    }
    def iin[T <: Data](data:Data) = {
      if(debug) {
        val dl = data.getBitsWidth
        InBuf += (data -> (dl,inAlloc))
        inAlloc += dl+1
      }
    }
    def out[T <: Data](dataType: HardType[T]) = {
      val r = cloneOf(dataType)
      r.asOutput
      if(debug){
        val dl = r.getBitsWidth
        IoOutBuf += (r->(dl,ioOutAlloc))
        ioOutAlloc += dl
      }
      r
    }
    def iout[T <: Data](data:Data) = {
      if(debug) {
        val dl = data.getBitsWidth
        if(DataInIo.contains(data)) {
          IoOutBuf += (data->(dl,ioOutAlloc))
          ioOutAlloc += dl  
        } else{
          OutBuf += (data->(dl,outAlloc))
          outAlloc += dl
        }
      }
    }
    def Inject:List[injection] = {
      def inj(p:String,s:Int,b:ArrayBuffer[(String,(Int,Int,Option[Debugable]))]):List[injection] = {
        b.foldLeft(List[injection]()){ case (l,(n,(dl,pos,o))) => o match {
            case None => injection(p+"/"+n,s+pos+dl,s+pos,dl) :: l
            case Some(dbo) => inj(p+"/"+n,s+pos,dbo.db.SignalIn) ::: l
          }
        }
      }
      inj("",0,SignalIn)
    }
    def Capture:List[captured] = {
      def cap(p:String,s:Int,b:ArrayBuffer[(String,(Int,Int,Option[Debugable]))]):List[captured] = {
        b.foldLeft(List[captured]()){ case (l,(n,(dl,pos,o))) => o match {
            case None => captured(p+"/"+n,s+pos,dl) :: l
            case Some(dbo) => cap(p+"/"+n,s+pos,dbo.db.SignalOut) ::: l
          }
        }
      }
      cap("",0,SignalOut)
    }
    def DebugItem:Map[String,debugitem] = {
      var items = ArrayBuffer[(String,debugitem)]()
      Inject.foreach(item => items += ((item.name+"_i") -> item))
      Capture.foreach(item => items += ((item.name+"_c") -> item))
      items.toMap
    }
    def inject[T <: Data] (data: T): T = {
      if(!debug) data else {
        def findIt(dl:Int,pos:Int,s:String):T = {
          val temp = cloneOf(data)
          temp.assignFromBits(dbIn(pos+dl-1 downto pos))
          SignalIn += (s -> (dl,pos,None)) 
          dbIn(pos+dl).asBits(0) ? temp | data
        }
        InBuf.toMap.get(data) match {
          case None => data 
          case Some((dl,pos)) =>  {
            val s = obj2name(data) 
            s match {
              case None => data
              case Some(s) => findIt(dl,pos,s)
            }
          }
        }
      }
    }
    def finalDb: dbBundle = {
      val dbP = dbBundle(inAlloc+inLen,outAlloc+outLen)
      if(inAlloc > 0) dbIn := dbP.inject(inAlloc-1 downto 0)
      var accumulateWidth = inAlloc
      subDebugs.foreach{ case (n,o) =>
        if(o.dbPort.aIn > 0) {
          o.dbPort.inject := dbP.inject(accumulateWidth+o.dbPort.aIn-1 downto accumulateWidth)
          SignalIn += (n -> (o.dbPort.aIn,accumulateWidth,Some(o)))
          accumulateWidth += o.dbPort.aIn
        }
      }
      OutBuf.foreach{ case(data,(dl,pos)) if(dl > 0) =>
        dbP.capture(pos+dl-1 downto pos) := data.asBits
        DataInCom.get(data) match {
          case None => throw new Exception(s"Can not find signal $data")
          case Some(n) => SignalOut += (n -> (dl,pos,None))
        }
      }
      accumulateWidth = outAlloc
      subDebugs.foreach{ case (n,o) =>
        o.db.IoOutBuf.foreach{ case (s,_) =>
          val dl = s.getBitsWidth
          if(dl > 0) { 
            dbP.capture(accumulateWidth+dl-1 downto accumulateWidth) := s.asBits
            o.db.obj2name(s) match {
              case None =>
              case Some(sn) => SignalOut += (n+"/"+sn -> (dl,accumulateWidth,None))
            }
            accumulateWidth += dl
          }
        }
        val dl = o.dbPort.aOut
        if(dl > 0){
          dbP.capture(accumulateWidth+dl-1 downto accumulateWidth) := o.dbPort.capture
          SignalOut += (n -> (dl,accumulateWidth,Some(o)))
          accumulateWidth += dl
        }
      }
      dbP
    }
  }

  def subDebugs: ArrayBuffer[(String, Debugable)] = {
    var modules = ArrayBuffer[(String, Debugable)]()
    Misc.reflect(this, (name, obj) => {
      obj match {
        case data: Debugable =>
          modules += (name -> data)
        case _ =>
      }
    })
    modules
  }
  def inLen:  Int = subDebugs.map{ case (n,o) => o.dbPort.aIn }.sum 
  def outLen: Int = subDebugs.map{ case (n,o) => o.db.ioOutAlloc + o.dbPort.aOut }.sum
}

object DebugUtils {
  def buildInject(dbin:List[(String,BigInt)],items:Map[String,debugitem]):BigInt = {
    var ret = BigInt(0)
    val mdbin = dbin.toMap
    items.foreach{ case (_,v) => v match {
        case injection(name,active,pos,dl) if mdbin.contains(name) =>  {
          ret |= BigInt(1) << active
          ret |= (mdbin(name) & ((BigInt(1) << dl) - 1)) << pos
        }
        case _ =>
      }
    }
    ret
  }
  def Capture2Signal(cap:BigInt,items:Map[String,debugitem]):Map[String,BigInt] = {
    var ret = ArrayBuffer[(String,BigInt)]()
    items.foreach{ case (_,v) => v match {
        case captured(name,pos,dl) => {
          ret += (name -> ((cap >> pos) & ((BigInt(1) << dl) - 1)))
        }
        case _ =>
      }
    }
    ret.toMap
  }
}

import scala.collection.mutable
case class DebugDelay[T](delay:Int,zero:T) {
  val q = new mutable.Queue[T]
  for(i <- 0 until delay) {
    q += zero
  }   
  def fifo(e:T):T = {
    q += e
    q.dequeue()
  }
}