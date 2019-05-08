package open5g.lib.tsp

import spinal.core._
import spinal.lib._

/**
 *  Timing Sequence Processor(TSP)
 *
 *  a processor
 *  has N thread
 *  every thread has M stage
 *  every stage have 
 *    a PC (IWidth)
 *    a state (SWidth) can automate dec by instruction
 *    a flag (FWidth)
 *  every clock run one instruction
 *  format of instruction
 *    auto Bool
 *    op   Bits(OWidth bits)
 *      set
 *      exec
 *        gpio
 *        DMA
 *        trigger
 *        
 *      pcc
 *        jump
 *        call
 *        intr
 *        ret
 *        reti 
 */