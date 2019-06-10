package open5g.lib.zcpsm.tools

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.collection.mutable.{ArrayBuffer,Map}

object PSM {
  val program : ArrayBuffer[asm] = ArrayBuffer[asm]()
  val labeled : Map[String,Int] = Map[String,Int]()
}

trait asm {
  val opName : String
  def toHex : Int = 0
  val opc : Int
  val opg : Int
  val ins : Int
}

trait opd {
  def toString : String
}

case class reg(r:Int) extends opd {
  var alloc : Int = r
  override def toString = f"s$alloc%02X"
}

case class imm(d:Int) extends opd {
  var data : Int = d
  override def toString = f"$data%02X"
}

abstract class twoArg(f:reg,s:opd) extends asm {
  override def toString = opName + " " + f.toString + ", " + s.toString
}

class alu(val opName:String,f:reg,s:opd,val opc:Int) extends twoArg(f,s) {
  val opg = 0xc
  val ins = 0xc000
}

class shift(val opName:String,r:reg,val opc:Int) extends asm {
  val opg = 0xd
  val ins = 0xd000
  override def toString = opName + " " + r.toString
}

trait programControl extends asm {
  val jumpG : Int
  val opc = 0
  val opg = 8
}

trait hasLabel extends programControl {
  val labelName:String
  def dest = PSM.labeled(labelName)
  override def toString = opName + " " + labelName
}

case class jumpUC(labelName:String) extends hasLabel {
  val opName = "JUMP"
  val jumpG = 0
  val ins = 0x9000
}

case class call(labelName:String) extends hasLabel {
  val opName = "CALL"
  val jumpG = 3
  val ins = 0x8c00
}
case class ret() extends programControl{
  val opName = "RETURN"
  val jumpG = 2
  val ins = 0x8800
  override def toString = opName
} 

abstract class conditionJump(val opName:String,val jumpF:Int, val ins:Int) extends hasLabel {
  val jumpG = 4
}

case class  jumpz(labelName:String) extends conditionJump("JUMP Z,", 0,0x8000) 
case class jumpnz(labelName:String) extends conditionJump("JUMP NZ,",1,0x8400) 
case class  jumpc(labelName:String) extends conditionJump("JUMP C,", 2,0x8800) 
case class jumpnc(labelName:String) extends conditionJump("JUMP NC,",3,0x8c00) 
  
case class  add(f:reg,s:opd) extends alu("ADD",  f,s,0)
case class addc(f:reg,s:opd) extends alu("ADDCY",f,s,1)
case class  sub(f:reg,s:opd) extends alu("SUB",  f,s,2)
case class subc(f:reg,s:opd) extends alu("SUBCY",f,s,3)
case class load(f:reg,s:opd) extends alu("LOAD", f,s,4)
case class  and(f:reg,s:opd) extends alu("AND",  f,s,5)
case class   or(f:reg,s:opd) extends alu("OR",   f,s,6)
case class  xor(f:reg,s:opd) extends alu("XOR",  f,s,7)

class inout(val opName:String,f:reg,s:opd,val opc:Int,val opg:Int, val ins:Int) extends twoArg(f,s)
case class  input(f:reg,s:opd) extends inout("INPUT",f,s,0,0xa,0xa000) 
case class output(f:reg,s:opd) extends inout("OUTPUT",f,s,0,0xe,0xe000) 

case class  sr0(r:reg) extends shift("SR0",r,0xe)
case class  sr1(r:reg) extends shift("SR1",r,0xf)
case class  srx(r:reg) extends shift("SRX",r,0xa)
case class  sra(r:reg) extends shift("SRA",r,0x8)
case class   rr(r:reg) extends shift("RR", r,0xc)
case class  sl0(r:reg) extends shift("SL0",r,0x6)
case class  sl1(r:reg) extends shift("SL1",r,0x7)
case class  slx(r:reg) extends shift("SLX",r,0x4)
case class  sla(r:reg) extends shift("SLA",r,0x0)
case class   rl(r:reg) extends shift("RL", r,0x2)

class asmLexer extends StdLexical {
  override type Elem = Char
  override def digit = ( super.digit | hexDigit )
  lazy val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  lazy val hexDigit = elem("hex digit", hexDigits.contains(_))
}

class asmParser extends StandardTokenParsers {
  override val lexical:asmLexer = new asmLexer()
  // import lexical.hexDigit
  def removeComment(s:Iterator[String]) = {
    s.map{x => {
        val i = x.indexOf(";;")
        if(i == -1) x else x.take(i)
      }
    }
  }
  lexical.delimiters += (":",",")
  lexical.reserved   += (
    "LOAD","AND","OR","XOR",
    "ADD","ADDCY","SUB","SUBCY",
    "SR0","SR1","SRX","SRA","RR",
    "SL0","SL1","SLX","SLA","RL",
    "INPUT","OUTPUT",
    "CALL","RETURN","JUMP")
  def label : Parser[String] = ident~":" ^^ {_.toString}
  def Reg : Parser[reg] = "s"~numericLit ^^ {case s~x => reg(Integer.parseInt(x.drop(1))) }
  def Imm : Parser[imm] = numericLit ^^ { x => imm(Integer.parseInt(x.toString,16)) }
  def Opd : Parser[opd] = (Reg|Imm)
  def TwoArg : Parser[asm] = ("LOAD"|"AND"|"OR"|"XOR"|"ADD"|"ADDCY"|"SUB"|"SUBCY"|"INPUT"|"OUTPUT") ~ Reg ~ "," ~ Opd ^^ {
    case o~r~c~d => o match {
      case "LOAD"   =>   load(r,d)
      case "AND"    =>    and(r,d)
      case "OR"     =>     or(r,d)
      case "XOR"    =>    xor(r,d)
      case "ADD"    =>    add(r,d)
      case "ADDCY"  =>   addc(r,d)
      case "SUB"    =>    sub(r,d)
      case "SUBCY"  =>   subc(r,d) 
      case "INPUT"  =>  input(r,d)
      case "OUTPUT" => output(r,d)
      case _        =>  and(reg(0),reg(0))
    }
  }
  def Shift : Parser[asm] = ("SR0"|"SR1"|"SRX"|"SRA"|"RR"|"SL0"|"SL1"|"SLX"|"SLA"|"RL") ~ Reg ^^ {
    case o~r => o match {
      case "SR0"    =>   sr0(r)
      case "SR1"    =>   sr1(r)
      case "SRX"    =>   srx(r)
      case "SRA"    =>   sra(r)
      case "RR"     =>    rr(r)
      case "SL0"    =>   sl0(r)
      case "SL1"    =>   sl1(r)
      case "SLX"    =>   slx(r) 
      case "SLA"    =>   slx(r)
      case "RL"     =>    rl(r)
      case _        =>  and(reg(0),reg(0))
    }
  }
  def Call : Parser[asm] = "CALL"~ident ^^ {case c~i => call(i.toString)}
  def Ret  : Parser[asm] = "RETURN" ^^ {_ => ret()}
  def Jump : Parser[asm] = "JUMP"~opt("Z,"|"NZ,"|"C,"|"NC,")~ident ^^ { case j~c~l => 
    val lb = l.toString
    c match {
      case None        => jumpUC(lb)
      case Some("Z,")  =>  jumpz(lb)
      case Some("NZ,") => jumpnz(lb)
      case Some("C,")  =>  jumpc(lb)
      case Some("NC,") => jumpnc(lb)
      case _           => and(reg(0),reg(0))
    }
  }
  def line : Parser[Any] = (TwoArg|Shift|Call|Ret|Jump|label)
  def psmParser : Parser[Any] = rep(line)
  def parserAll[T]( p : Parser[T], input :String) = {
    phrase(p)( new lexical.Scanner(input))
  }
}