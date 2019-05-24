package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import open5g.lib.usrp._

object MyNocGen {
  def main(args: Array[String]) {
    SpinalVerilog(new noc_responder())
  }
} 