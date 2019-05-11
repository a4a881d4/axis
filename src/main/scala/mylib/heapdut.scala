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
      while(idx<520) {
        dut.io.clear #= false
        dut.io.insert.payload.key #= idx
        dut.io.insert.value #= 0
        dut.clockDomain.waitRisingEdge()
        val ready = dut.io.insert.ready.toBoolean
        val size = dut.io.size.toInt
        println(s"$idx,$ready,$size")
        idx += 1
      }
    }
  }
} 
