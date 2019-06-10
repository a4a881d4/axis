package open5g.lib.zcpsm.tools

import scala.util.parsing.combinator.RegexParsers
import scala.collection._

object PSM {
  val program : mutable.ArrayBuffer[asm] = mutable.ArrayBuffer[asm]()
  val labeled : mutable.Map[String,Int] = mutable.Map[String,Int]()
  def bin = program.toList.map(_.toHex)
  def disAsm(b:List[Int]) : (List[asm],Map[String,Int]) = {
    program.clear()
    labeled.clear()
    for(i <- b) {
      val bI = binIns(i)
      program += bI.disAsm
      if(bI.hasLabel) {
        labeled += (bI.label -> bI.dest)
      }
    }
    (program.toList,labeled)
  }
}

trait asm {
  val opName : String
  def toHex : Int
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
  val fisrt  = (alloc&0xf)<<8 | ((alloc&0x10)>>4)<<17
  val second = (alloc&0xf)<<4 | ((alloc&0x10)>>4)<<16
}

case class binIns(i:Int) {
  def  firstReg = ((i>>17)&1)<<4|((i>>8)&0xf)
  def secondReg = ((i>>16)&1)<<4|((i>>4)&0xf)
  def   immData = i&0xff
  def immOpCode = (i>>12)&0x7
  def regOpCode = i&7
  def shtOpCode = i&0xf
  def   opGroup = (i>>12)&0xf
  def progContr = (i>>8) &0xc
  def      dest = i&0x3ff | ((i>>16)&3)<<10
  def     label = s"L$dest"
  def disAsm:asm = opGroup match {
    case 0xf => output(reg(firstReg),reg(secondReg))
    case 0xe => output(reg(firstReg),imm(immData))
    case 0xd => shift.opc2asm(shtOpCode,reg(firstReg))
    case 0xc => alu.opc2asm(regOpCode,reg(firstReg),reg(secondReg))
    case 0xb => input(reg(firstReg),reg(secondReg))
    case 0xa => input(reg(firstReg),imm(immData))
    case 0x9 => conditionJump.opc2asm(progContr,label)
    case 0x8 => programControl.opc2asm(progContr,label)
    case x if x<8 => alu.opc2asm(regOpCode,reg(firstReg),imm(immData))
    case _   => and(reg(0),reg(0))
  }
  def hasLabel = opGroup match {
    case 0x9 => true
    case 0x8 => progContr match {
      case 0x0|0xc => true
      case _ => false
    }
    case _ => false
  } 
}

case class imm(d:Int) extends opd {
  var data : Int = d
  override def toString = f"$data%02X"
  def hex = data&0xff
}

abstract class twoArg(f:reg,s:opd) extends asm {
  override def toString = opName + " " + f.toString + ", " + s.toString
}

object alu {
  def opc2asm(opc:Int,f:reg,s:opd) = opc match {
    case 0 => load(f,s)
    case 1 =>  and(f,s)
    case 2 =>   or(f,s)
    case 3 =>  xor(f,s)
    case 4 =>  add(f,s)
    case 5 => addc(f,s)
    case 6 =>  sub(f,s)
    case 7 => subc(f,s)
    case _ => and(reg(0),reg(0))
  }
}

class alu(val opName:String,f:reg,s:opd,val opc:Int) extends twoArg(f,s) {
  val opg = 0xc
  val ins = 0xc000
  def toHex = {
    s match {
      case r:reg => f.fisrt | (opc&0x7)     | r.second | 0xc000
      case i:imm => f.fisrt | (opc&0x7)<<12 | i.hex
    }
  }
}

object shift {
  def opc2asm(opc:Int,r:reg) = opc match {
    case 0xe => sr0(r)
    case 0xf => sr1(r)
    case 0xa => srx(r)
    case 0x8 => sra(r)
    case 0xc =>  rr(r)
    case 0x6 => sl0(r)
    case 0x7 => sl1(r)
    case 0x4 => slx(r)
    case 0x0 => sla(r)
    case 0x2 =>  rl(r)
    case _   => sr0(reg(0))
  } 
}

class shift(val opName:String,r:reg,val opc:Int) extends asm {
  val opg = 0xd
  val ins = 0xd000
  override def toString = opName + " " + r.toString
  def toHex = 0xd000 | r.fisrt | opc&0xf
}

class inout(val opName:String,f:reg,s:opd,val opc:Int,val opg:Int, val ins:Int) extends twoArg(f,s) {
  def toHex = s match {
    case r:reg => f.fisrt | r.second | (ins+0x1000)
    case i:imm => f.fisrt | i.hex    | (ins+0x0000)
  }
}

abstract class programControl(val opName:String, val jumpG:Int, val ins:Int) extends asm {
  val opc = 0
  val opg = 8
}

object programControl {
  def opc2asm(opc:Int,l:String) = opc match {
    case 0x0 => jumpUC(l)
    case 0x8 =>    ret()
    case 0xc =>   call(l)
    case _   => and(reg(0),reg(0))
  }
}

object conditionJump {
  def opc2asm(opc:Int,l:String) = opc match {
    case 0x0 =>  jumpz(l)
    case 0x4 => jumpnz(l)
    case 0x8 =>  jumpc(l)
    case 0xc => jumpnc(l)
    case _   => and(reg(0),reg(0))
  }
}

trait hasLabel {
  val ins : Int
  val opName : String
  val labelName:String
  def dest = PSM.labeled(labelName)
  override def toString = opName + " " + labelName
  def toHex = ins | dest&0x3ff | ((dest&0xc00)>>10)<<16
}

case class jumpUC(labelName:String) extends programControl("JUMP",0,0x8000) with hasLabel
case class call(labelName:String)   extends programControl("CALL",3,0x8c00) with hasLabel
case class ret() extends programControl("RETURN",2,0x8800){
  override def toString = opName
  def toHex = ins
} 

trait conditionJump extends hasLabel {
  val ins:Int
  def jumpF = (ins-0x9000)/0x400
  def cond = jumpF match {
    case 0 => "Z"
    case 1 => "NZ"
    case 2 => "C"
    case 3 => "NC"
    case _ => ""
  }
  override def toString = opName + " " + cond + ", " + labelName
}

case class  jumpz(labelName:String) extends programControl("JUMP",4,0x9000)  with conditionJump
case class jumpnz(labelName:String) extends programControl("JUMP",4,0x9400)  with conditionJump
case class  jumpc(labelName:String) extends programControl("JUMP",4,0x9800)  with conditionJump
case class jumpnc(labelName:String) extends programControl("JUMP",4,0x9c00)  with conditionJump
  
case class load(f:reg,s:opd) extends alu("LOAD", f,s,0)
case class  and(f:reg,s:opd) extends alu("AND",  f,s,1)
case class   or(f:reg,s:opd) extends alu("OR",   f,s,2)
case class  xor(f:reg,s:opd) extends alu("XOR",  f,s,3)
case class  add(f:reg,s:opd) extends alu("ADD",  f,s,4)
case class addc(f:reg,s:opd) extends alu("ADDCY",f,s,5)
case class  sub(f:reg,s:opd) extends alu("SUB",  f,s,6)
case class subc(f:reg,s:opd) extends alu("SUBCY",f,s,7)

case class  input(f:reg,s:opd) extends inout("INPUT", f,s,0,0xa,0xa000) 
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

case class label_(name:String)

class asmParser extends RegexParsers {
  def removeComment(s:Iterator[String]) = {
    s.map{x => {
        val i = x.indexOf(";;")
        if(i == -1) x else x.take(i)
      }
    }
  }

  def ident : Parser[String] = """[a-zA-Z_]\w*""".r
  def label : Parser[label_] = ident<~":" ^^ {case x:String => label_(x)}
  def numericLit : Parser[String] = """[0-9a-fA-F]*""".r
  def Reg : Parser[reg] = """s[0-1][0-9a-fA-F]""".r ^^ {x => reg(Integer.parseInt(x.drop(1),16)) }
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
  def psmParser : Parser[List[Any]] = rep(line) ^^ (_.toList)
  def fromFile(asmString:String) : (List[asm],Map[String,Int]) = {
    PSM.program.clear()
    PSM.labeled.clear()
    val rC = removeComment(asmString.split("\n").toIterator).filter(_!="").reduce(_+"\n"+_)
    parseAll(psmParser,rC) match {
      case Success(result,_) => {
        for(e <- result) {
          e match {
            case l:label_ => PSM.labeled += (l.name -> PSM.program.length)
            case a:asm    => PSM.program += a
            case _ =>
          }
        }
      }
      case _ => 
    }
    (PSM.program.toList,PSM.labeled)
  }
  def toFile = {
    val rL = PSM.labeled.map{case (a,b) => (b->a)}
    var r = ""
    for(i <- 0 until PSM.program.length) {
      if(rL.contains(i)) r += rL(i) + ":\n"
      r += PSM.program(i).toString + "\n"
    }
    r
  }
}
