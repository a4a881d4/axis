package open5g.lib.tsp

import spinal.core._
import spinal.lib._

import open5g.lib.common.Constant
/**
 *  Timing Sequence Processor(TSP)
 *
 *  a processor
 *  has NThread threads (log2Up(NThread) = TWidth)
 *  every thread has MStage stages (log2Up(MStage) = SGWidth)
 *  every stage have 
 *    a PC (IWidth)
 *    a APC (new Bool, ret Bool, PC IWidth bits)
 *    a state (SWidth) can automate dec by instruction
 *    a flag (FWidth)
 *  every clock run one instruction
 *  format of instruction
 *    auto Bool
 *    mask 3 bits
 *    exec Bool
 *      
 *      alu B(0)
 *        loadr
 *        loadf
 *        
 *      exec B(1)
 *        gpio
 *        DMAD
 *        DMAR
 *        trigger
 *
 *      pcc B(3)
 *        jump
 *        calln
 *        calla
 *        intr
 *        ret
 *        reti
 *
 *    direct Bits(DWidth bits)
 *      loadr   B(0,4 bits) ## (mode 9 bits) + pad + (dir 16 bits) * 3 // direct, reg, gpio signal, interal signal
 *      aluop   B(1,4 bits) ## (mode 9 bits) + pad + (dir 16 bits) * 3 // add sub shift logic| opA 2 bits| opB 2 bits| result
 *      loadf   B(2,4 bits) ## (mode 3 bits) * 3 + pad + (mask 16 bits) * 3 // 3 group, 16 signal pre group
 *      loadg   B(3,4 bits) ## (mode 3 bits) * 3 + pad + (mask 16 bits) * 3
 *
 *      gpio    B(0,4 bits) ## (mode 2 bits, 00 reset, 10 set, 01 flip, 11 nop) * (DWidth/2-1)
 *      trigger B(1,4 bits) ## (signal DWidth-2 bits)
 *      DMAD    B(2,4 bits) ## (DMA_ch 4 bits, from 12, to 12, len 9 )
 *      CHDR    B(3,4 bits) ## (DMA_ch 4 bits, dest 16 bits, src 16 bits, type 4 bits, addr 17 )
 *      
 *      jumps   B(1,4bits)  ## (stage SGWidth bits, pad DWidth-4-SGWidth-IAWidth bits, addr IAWidth bits)
 *      jumpf   B(2,4bits)  ## (flag 16 bits, pad DWidth-4-16-IAWidth bits, addr IAWidth bits)
 *      jumpr   B(3,4bits)  ## (flag 16 bits, pad DWidth-4-16-IAWidth bits, addr IAWidth bits)
 *      callf   B(4,4bits)  ## (flag 16 bits, pad DWidth-4-16-IAWidth bits, addr IAWidth bits)
 *      calla   B(5,4bits)  ## (arg 16 bits, addr IAWidth bits)
 *      intr    B(8,4bits)  ## (thread TWidth bits, pad DWidth-4-TWidth-IAWidth bits, addr IAWidth bits)
 *      ret     B(9,4bits)  ## (pad DWidth-4 bits)
 *      reti    B(10,4bits) ## (pad DWidth-4 bits)
 *
 *
 */

case class TSPConfig( NThread : Int,
                      MStage  : Int,
                      IWidth  : Int,
                      RWidth  : Int,
                      IAWidth : Int,
                      SWidth  : Int
                      ) {
  def TWidth  = log2Up(NThread)
  def SGWidth = log2Up(MStage)
  def FWidth  = RWidth - IAWidth - (3*SWidth)
  def PWidth  = IWidth - 10
}

object TSPConfig {
  def default = TSPConfig(32,16,72,72,9,16)
}

class FlagIO(cfg:TSPConfig) extends Bundle with IMasterSlave {
  val carry = Bool
  val zero  = Bool
  val state = Bits(3 bits)
  val gpio  = Bits(3 bits)
  val other = Bits(cfg.FWidth-8 bits)
  def asMaster = {
    out(carry,zero,state,gpio,other)
  }
}

class RegIO(cfg:TSPConfig) extends Bundle with IMasterSlave {
  val pc = UInt(cfg.IAWidth bits)
  val state = Vec(UInt(cfg.SWidth bits),3)
  val flag  = new FlagIO(cfg)
  def asMaster = {
    out(pc,state,flag)
  }
}

class Instruction(cfg:TSPConfig) extends Bundle with IMasterSlave {
  val auto   = Bool
  val mask   = Bits(3 bits)
  val exec   = Bool
  val pluged = Bits(cfg.PWidth bits)
  def asMaster = {
    out(auto,mask,exec,pluged)
  }
}

object Instruction {
  val nop  = Constant(0,2)
  val alu  = Constant(1,2)
  val exec = Constant(2,2)
  val pcc  = Constant(3,2)
}