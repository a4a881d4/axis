package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import open5g.lib.heap._
import open5g.lib.common.SignalFormat
object MyHeapRandom {
  def main(srgs: Array[String]) {
    val config = HeapConfig(64,HeapItemConfig(24,12),16)
    SimConfig.withWave.doSim(new heap(config,false)){dut =>
      var idx = 0
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.clear #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.clear #= false
      dut.io.insert.valid #= false
      while(idx<65536) {
        dut.io.output.ready #= true
        dut.io.now #= idx
        if(!dut.io.insert.valid.toBoolean && Random.nextInt(100)<5) {
          val insertD = idx + Random.nextInt(1024) 
          dut.io.insert.payload.key #= insertD
          dut.io.insert.payload.value #= 0
          dut.io.insert.valid #= true
        }
        dut.clockDomain.waitRisingEdge()
        val size = dut.io.size.toInt
        val state = dut.io.state.toInt
        if(dut.io.insert.valid.toBoolean && dut.io.insert.ready.toBoolean) {
          dut.io.insert.valid #= false
          val v = dut.io.insert.payload.key.toInt
          println(s"[$idx:$size($state)] insert: $v")
        }
        if(dut.io.output.valid.toBoolean) {
          val v = dut.io.output.key.toInt
          println(s"[$idx:$size($state)] pop $v delay = ${idx-v}")
        }
        idx += 1
      }
    }
  }
}

object MyHeapSim {
  def toInt(x:Boolean) = if(x) 1 else 0
  def main(args: Array[String]) {

    val config = HeapConfig(64,HeapItemConfig(24,12),16)
    val f = SignalFormat()
    f.add("idx",5)
    f.add("state",4)
    f.add("ready",3)
    f.add("size",4)
    f.add("id",5)
    f.add("ov",3)
    f.add("out",5)
    f.add("wen",3)
    f.add("wa",5)
    f.add("wd",5)
    f.add("ra",5)
    f.add("rd",5)
    f.add("left",5)
    f.add("right",5)
    def output(dut:heap,idx:Int) : String = {
      val toDebug = Map(
        "idx"   -> idx,
        "state" -> dut.io.state.toInt,
        "size"  -> dut.io.size.toInt,
        "ready" -> toInt(dut.io.insert.ready.toBoolean),
        "id"    -> dut.debug.data.toInt,
        "out"   -> dut.io.output.payload.key.toInt,
        "ov"    -> toInt(dut.io.output.valid.toBoolean),
        "wen"   -> toInt(dut.debug.wen.toBoolean),
        "wa"    -> dut.debug.wa.toInt,
        "wd"    -> dut.debug.wd.toInt,
        "ra"    -> dut.debug.ra.toInt,
        "rd"    -> dut.debug.rd.toInt,
        "left"  -> dut.debug.left.toInt,
        "right" -> dut.debug.right.toInt
        )
      val r = if(idx%10 == 0) f.TabHead + "\n" else ""
      r + f.Tab(toDebug)  
    }
    SimConfig.withWave.doSim(new heap(config,true)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      println(f.TabHead)
      var idx = 0
      var da  = 0

      dut.io.clear #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.insert.valid #= true
      dut.io.output.ready #= false
      while(idx<600) {
        dut.io.clear #= false
        dut.io.insert.payload.key #= 512-da
        dut.io.insert.value #= 0
        dut.clockDomain.waitRisingEdge()
        println(output(dut,idx))
        if(dut.io.insert.ready.toBoolean) {
          da += 1
        }
        idx += 1
      }
      idx = 0
      println(f.TabHead)
      while(idx<200) {
        dut.io.insert.valid #= false
        dut.io.output.ready #= true
        dut.io.now #= 508
        dut.clockDomain.waitRisingEdge()
        println(output(dut,idx))
        idx += 1
      }
      dut.io.clear #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.clear #= false
      idx = 0
      while(idx<200) {
        dut.io.insert.valid #= false
        dut.io.output.ready #= true
        dut.io.now #= 508
        dut.clockDomain.waitRisingEdge()
        println(output(dut,idx))
        idx += 1
      }
    }
  }
}

object MyHeapGen {
  def main(args: Array[String]) {
    val config = HeapConfig(64,HeapItemConfig(24,12),16)
    SpinalVerilog(new heap(config,false))
  }
} 
