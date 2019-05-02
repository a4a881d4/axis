package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import open5g.lib.ringbus._

object MyRBDutVerilog {
  def main(args: Array[String]) {
    val cfg = RingBusConfig(128)
    SpinalVerilog(RingBus(cfg,3))
  }
}