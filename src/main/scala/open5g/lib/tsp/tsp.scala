package open5g.lib.tsp

import spinal.core._
import spinal.lib._

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
 *    op   Bits(2 bits)
 *      nop B(0)
 *      alu B(1)
 *        loadr
 *        loadf
 *        
 *      exec B(2)
 *        gpio
 *        DMAD
 *        DMAR
 *        trigger
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
 *      jumps   B(0,4bits)  ## (stage SGWidth bits, pad DWidth-4-SGWidth-IAWidth bits, addr IAWidth bits)
 *      jumpf   B(1,4bits)  ## (flag 16 bits, pad DWidth-4-16-IAWidth bits, addr IAWidth bits)
 *      jumpr   B(2,4bits)  ## (flag 16 bits, pad DWidth-4-16-IAWidth bits, addr IAWidth bits)
 *      callf   B(3,4bits)  ## (flag 16 bits, pad DWidth-4-16-IAWidth bits, addr IAWidth bits)
 *      calla   B(4,4bits)  ## (arg 16 bits, addr IAWidth bits)
 *      intr    B(8,4bits)  ## (thread TWidth bits, pad DWidth-4-TWidth-IAWidth bits, addr IAWidth bits)
 *      ret     B(9,4bits)  ## (pad DWidth-4 bits)
 *      reti    B(10,4bits) ## (pad DWidth-4 bits)
 *
 *
 */

 case class TSPConfig()