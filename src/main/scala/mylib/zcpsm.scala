package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import open5g.lib.zcpsm._

object MyZ {
  def main(srgs: Array[String]) {

    SpinalVerilog(new zcpsm(12))
  }
}
