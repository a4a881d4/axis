package open5g.lib.zcpsm.tools

import scala.util.parsing.combinator.syntactical._
import scala.collection.mutable.{ArrayBuffer,Map}

object PSM {
  val program : ArrayBuffer[asm] = ArrayBuffer[asm]()
  val labeled : Map[String,Int] = Map[String,Int]()
}

trait asm {
  def toString : String
  def toHex : Int = 0
}

trait opd {
  def toString : String
}

class reg extends opd {
  var alloc : Int = 0
  override def toString = f"R$alloc%02X"
}

class imm extends opd {
  var data : Int = 0
  override def toString = f"$data%02X"
}

trait opCode {
  val opc : Int
  val opg : Int
  val ins : Int
}

class twoArg(f:reg,s:opd) extends asm

class alu(f:reg,s:opd,val opc:Int) extends twoArg(f,s) with opCode {
  val opg = 0xc
  val ins = 0xc000
}

class shift(r:reg,val opc:Int) extends asm with opCode {
  val opg = 0xd
  val ins = 0xd000
}

trait programControl extends asm with opCode {
  val jumpG : Int
  val opc = 0
  val opg = 8
}

trait hasLabel extends programControl {
  val labelName:String
  def dest = PSM.labeled(labelName)
}

trait hasCondition extends hasLabel {
  val jumpG = 4
  val jumpF : Int
}

case class jumpUC(labelName:String) extends hasLabel {
  val jumpG = 0
  val ins = 0x9000
}

case class jumpz(labelName:String) extends hasCondition {
  val jumpF = 0
  val ins = 0x8000
}

case class jumpnz(labelName:String) extends hasCondition {
  val jumpF = 1
  val ins = 0x8400
}

case class jumpc(labelName:String) extends hasCondition {
  val jumpF = 2
  val ins = 0x8800
}
case class jumpnc(labelName:String) extends hasCondition {
  val jumpF = 3
  val ins = 0x8c00
}

case class call(labelName:String) extends hasLabel {
  val jumpG = 3
  val ins = 0x8c00
}
case class ret() extends programControl{
  val jumpG = 2
  val ins = 0x8800
} 

case class  add(f:reg,s:opd) extends alu(f,s,0)
case class addc(f:reg,s:opd) extends alu(f,s,1)
case class  sub(f:reg,s:opd) extends alu(f,s,2)
case class subc(f:reg,s:opd) extends alu(f,s,3)
case class load(f:reg,s:opd) extends alu(f,s,4)
case class  and(f:reg,s:opd) extends alu(f,s,5)
case class   or(f:reg,s:opd) extends alu(f,s,6)
case class  xor(f:reg,s:opd) extends alu(f,s,7)

class inout(f:reg,s:opd,val opc:Int,val opg:Int, val ins:Int) extends twoArg(f,s) with opCode
case class  input(f:reg,s:opd) extends inout(f,s,0,0xa,0xa000) 
case class output(f:reg,s:opd) extends inout(f,s,0,0xe,0xe000) 

case class  sr0(r:reg) extends shift(r,0xe)
case class  sr1(r:reg) extends shift(r,0xf)
case class  srx(r:reg) extends shift(r,0xa)
case class  sra(r:reg) extends shift(r,0x8)
case class   rr(r:reg) extends shift(r,0xc)
case class  sl0(r:reg) extends shift(r,0x6)
case class  sl1(r:reg) extends shift(r,0x7)
case class  slx(r:reg) extends shift(r,0x4)
case class  sla(r:reg) extends shift(r,0x0)
case class   rl(r:reg) extends shift(r,0x2)

