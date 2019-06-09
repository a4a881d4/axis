package open5g.lib.zcpsm

import spinal.core._
import spinal.lib._

case class addc(width:Int) extends Component {
  val io = new Bundle {
    val opa = in  Bits(width bits)
    val opb = in  Bits(width bits)
    val ci  = in  Bool
    val sum = out Bits(width bits)
    val co  = out Bool
  }
  val sum = UInt(width+1 bits)
  sum := io.opa.resize(width+1).asUInt + io.opb.asUInt + io.ci.asUInt
  io.sum := sum(width-1 downto 0).asBits
  io.co := sum.msb
}

case class addsub(width:Int) extends Component {
  val io = new Bundle {
    val A,B = in Bits(width bits)
    val Ci,C_EN,SUB = in Bool
    val S = out Bits(width bits)
    val Co = out Bool
  }
  val ac = addc(width)
  ac.io.opb := Mux(io.SUB, ~io.B, io.B)
  ac.io.opa := io.A
  ac.io.ci  := (io.Ci && io.C_EN) ^ io.SUB
  io.Co     := ac.io.co ^ io.SUB
  io.S      := ac.io.sum
}

case class logical(width:Int) extends Component {
  val io = new Bundle {
    val A,B = in Bits(width bits)
    val OP = in Bits(2 bits)
    val S = out Bits(width bits)
  }
  io.S := io.OP.asUInt.mux(
    0 -> io.B,
    1 -> (io.A & io.B),
    2 -> (io.A | io.B),
    3 -> (io.A ^ io.B)
  )
}

case class shiftL(width:Int) extends Component {
  val io = new Bundle {
    val A = in Bits(width bits)
    val Ci = in Bool
    val OP = in Bits(3 bits)
    val S = out Bits(width bits)
    val Co = out Bool
  }
  io.S(width-1 downto 1) := io.A(width-2 downto 0)
  io.Co := io.A.msb
  io.S.lsb := io.OP.asUInt.mux(
    6 -> False,
    7 -> True,
    4 -> io.A.lsb,
    0 -> io.Ci,
    2 -> io.A.msb,
    default -> False
  )
}

case class shiftR(width:Int) extends Component {
  val io = new Bundle {
    val A = in Bits(width bits)
    val Ci = in Bool
    val OP = in Bits(3 bits)
    val S = out Bits(width bits)
    val Co = out Bool
  }
  io.S(width-2 downto 0) := io.A(width-1 downto 1)
  io.Co := io.A.lsb
  io.S.msb := io.OP.asUInt.mux(
    6 -> False,
    7 -> True,
    4 -> io.A.lsb,
    0 -> io.Ci,
    2 -> io.A.msb,
    default -> False
  )
}

case class StackP(width:Int) extends Component {
  val io = new Bundle {
    val en = in Bool
    val pop_push = in Bool
    val addr = out UInt(width bits)
  }
  val count = Reg(UInt(width bits)) init(0)
  val updown = Mux(io.pop_push, count+1, count-1)
  when(io.en) {
    count := updown
  }
  io.addr := Mux(io.pop_push, updown, count)
}

case class zRegFile(AWidth:Int,DWidth:Int) extends Component {
  val io = new Bundle {
    val addra,addrb = in UInt(AWidth bits)
    val dia = in Bits(DWidth bits)
    val wea = in Bool
    val doa,dob = out Bits(DWidth bits)
  }
  val rf = Mem(Bits(DWidth bits), wordCount = (1 << AWidth))
  rf.write(address = io.addra, enable = io.wea, data = io.dia)
  io.doa := rf(io.addra)
  io.dob := rf(io.addrb)
}
 
case class zcpsm() extends Component {
  val io = new Bundle {
    val address = out UInt(12 bits)
    val instruction = in Bits(18 bits)
    val port_id = out Bits(8 bits)
    val write_strobe,read_strobe = out Bool
    val out_port = out Bits(8 bits)
    val in_port = in Bits(8 bits)
  }

  val rf = zRegFile(5,8)
  val sL = shiftL(8)
  val sR = shiftR(8)
  val lo = logical(8)
  val as = addsub(8)
  val sp = StackP(5)
  val stack = Mem(UInt(12 bits),wordCount = 32)

  val ins = RegInit(B"001100000000000000") 
  val pc = Reg(UInt(12 bits)) init 0
  val nextPc = pc + 1
  val jumpSet = RegInit(False)
  val stack_dout = stack(sp.io.addr)
  val stack_en = (ins(15 downto 13) === B"100") && (ins(12 downto 11) === B"01")
  val stack_din = pc - 1
  val cflag,zflag = RegInit(False)
  val jumpFlag = Mux(ins(11), (cflag ^ ins(10)), (zflag ^ ins(10)))
  val alu_out = Bits(8 bits)
  val alu_cflag = Bool

  sp.io.pop_push := ins(10)
  sp.io.en := stack_en 
  stack.write(address = sp.io.addr, enable = stack_en, data = stack_din)

  when(ins(15 downto 13) === B"100") {
    when( (jumpFlag && ins(12)) ||
          (ins(12 downto 10) === B"000") ||
          (ins(12 downto 10) === B"011")
      ) {
      pc := (ins(17 downto 16) ## ins(9 downto 0)).asUInt
      jumpSet := False 
      ins := B"001100000000000000"
    }.elsewhen(ins(12 downto 10) === B"010") {
      pc := stack_dout
      jumpSet := False
      ins := B"001100000000000000"
    }.otherwise {
      pc := nextPc
      jumpSet := True 
      when(jumpSet) {
        ins := io.instruction
      } otherwise {
        ins := B"001100000000000000"
      }
    }
  } otherwise {
    pc := nextPc
    jumpSet := True 
    when(jumpSet) {
      ins := io.instruction
    } otherwise {
      ins := B"001100000000000000"
    }
  }

  val read_strobe = (ins( 15 downto 13 ) === B"101")
  io.read_strobe := read_strobe
  val write_strobe = (ins( 15 downto 13 ) === B"111")
  io.write_strobe := write_strobe

  rf.io.addra := (ins(17) ## ins(11 downto 8)).asUInt
  rf.io.addrb := (ins(16) ## ins(7 downto 4)).asUInt
  rf.io.dia :=  Mux(read_strobe,io.in_port,alu_out)
  val wea = (ins(15 downto 13 ) =/= B"100") && (ins(15 downto 13 ) =/= B"111")
  rf.io.wea := wea

  io.port_id := Mux(ins(12), rf.io.dob, ins(7 downto 0))
  val ALU_OP = Mux(ins(15), ins(2 downto 0), ins(14 downto 12))
  val SHIFT_OP = ins(3 downto 0)
  val ALU_A = rf.io.doa
  val ALU_B = Mux(ins(15), rf.io.dob, ins(7 downto 0))
  val SHIFT_SEL = ins(15 downto 12 ) === B"1101"

  when(!SHIFT_SEL) {
    when(ALU_OP(2)) {
      alu_out := as.io.S
      alu_cflag := as.io.Co
    } otherwise {
      alu_out := lo.io.S
      alu_cflag := False
    } 
  } otherwise {
    when(SHIFT_OP(3)) {
      alu_out := sR.io.S
      alu_cflag := sR.io.Co
    } otherwise {
      alu_out := sL.io.S
      alu_cflag := sL.io.Co
    }
  }

  when(wea && ALU_OP =/= B"000") {
    cflag := alu_cflag
    when(alu_out === B(0,8 bits)) {
      zflag := True
    } otherwise {
      zflag := False
    }
  }
  io.address := pc
  io.out_port := ALU_A

  as.io.A := ALU_A
  as.io.B := ALU_B
  as.io.Ci := cflag
  as.io.C_EN := ALU_OP(0)
  as.io.SUB := ALU_OP(1)

  lo.io.A := ALU_A
  lo.io.B := ALU_B
  lo.io.OP := ALU_OP(1 downto 0)

  sL.io.A := ALU_A
  sL.io.Ci := cflag
  sL.io.OP := SHIFT_OP(2 downto 0)

  sR.io.A := ALU_A
  sR.io.Ci := cflag
  sR.io.OP := SHIFT_OP(2 downto 0)

}
