package mylib

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import open5g.lib.usrp._

object MyNocGen {
  def main(args: Array[String]) {
    SpinalVerilog(new noc_shell(NOC_ID = 0,
  INPUT_PORTS = 2,
  OUTPUT_PORTS = 2,
  USE_TIMED_CMDS  = false,
  STR_SINK_FIFOSIZE  = List.fill(2)(11),
  MTU = List.fill(2)(10),
  USE_GATE_MASK = 0,
  CMD_FIFO_SIZE  = List.fill(2)(5),
  RESP_FIFO_SIZE = 5))
  }
} 
