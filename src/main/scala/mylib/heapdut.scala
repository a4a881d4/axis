package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import open5g.lib.heap._


object MyHeapSim {
  def main(args: Array[String]) {
    val config = HeapConfig(512,HeapItemConfig(24,12))
    SimConfig.withWave.doSim(new heap(config)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var idx = 0

      dut.io.clear #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.insert.valid #= true
      while(idx<20) {
        dut.io.clear #= false
        dut.io.insert.payload.key #= 512-idx
        dut.io.insert.value #= 0
        dut.clockDomain.waitRisingEdge()
        val ready = dut.io.insert.ready.toBoolean
        val size = dut.io.size.toInt
        val wa = dut.debug.wa.toInt
        val ra = dut.debug.ra.toInt
        val wen = dut.debug.en.toBoolean
        val rd = dut.debug.rd.key.toInt
        val wd = dut.debug.wd.key.toInt
        val id = dut.debug.id.key.toInt
        println(s"$idx,$ready,$size,id=$id,ra=$ra,rd=$rd,$wen,wa=$wa,wd=$wd")
        if(ready) {
          idx += 1
        }
      }
    }
  }
} 
