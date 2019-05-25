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
  INPUT_PORTS = 1,
  OUTPUT_PORTS = 1,
  USE_TIMED_CMDS  = false,
  STR_SINK_FIFOSIZE  = List.fill(1)(11),
  MTU = List.fill(1)(10),
  USE_GATE_MASK = 0,
  BLOCK_PORTS = 1,
  CMD_FIFO_SIZE  = List.fill(1)(5),
  RESP_FIFO_SIZE = 0))
  }
} 
