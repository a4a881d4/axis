package open5g.lib.usrp

import spinal.core._
import spinal.lib._

class IBFGDS extends BlackBox {
  val io = new Bundle {
    val I = in Bool
    val IB = in Bool
    val O = out Bool
  }
  noIoPrefix()
}

import scala.util.parsing.combinator.syntactical._
trait ParserElem
class elemExpr extends ParserElem
case class elemIdent(name:String) extends ParserElem

case class elemModule(name:String,generics:List[elemGeneric],ports:List[elemPort]) extends ParserElem {
  override def toString = {
    "case class " + name + "(" + generics.map(_.toString).reduce(_ + ",\n" + _) + ")" + " extends BlackBox {\n" +
    "\tval io = new Bundle {\n" + ports.map(_.toString).reduce(_ + "\n" + _) + "\n" +
    "\t}\n" +
    "\tnoIoPrefix()\n" +
    "}\n" 
  }
}

case class elemGeneric(name:String,value:String) extends ParserElem {
  override def toString = {
    "\t" + name + " : Int = " + value
  }
}

case class elemPort(name:String,signal:String,dir:Int) extends ParserElem {
  override def toString = {
    "\t\t" + "val " + name + " = " + (if(dir == 0) "in" else "out") + " " + signal
  }
}

case class elemInPort(signal:String) extends ParserElem
case class elemOutPort(signal:String) extends ParserElem
case class elemRange(range:String) extends ParserElem

class verilogParser extends StandardTokenParsers {
  lexical.delimiters += (",","#","(",")",";","=","[","]",":","-","+","/","*")
  lexical.reserved   += ("module",
    "parameter",
    "input",
    "output",
    "wire",
    "reg")

  def parserModule : Parser[Any] = {
    "module"~ident~opt(parserGenerics)~parserPorts~";" ^^ { x => x match {
        case module~ident~Some(gen)~ports~com => elemModule(ident,gen,ports)
        case module~ident~None~ports~com      => elemModule(ident,List[elemGeneric](),ports)
      }
    }
  }
  def parserGenerics : Parser[List[elemGeneric]] ={
    "#"~"("~repsep(parserOneGeneric,",")~")" ^^ { x => x match {
      case j~c~gens~c2 => gens
      case _ => List[elemGeneric]() 
      }
    }
  }
  def parserOneGeneric : Parser[elemGeneric] = {
    "parameter"~ident~"="~parserExpr ^^ { case parameter~ident~e~value => elemGeneric(ident,value) }
  }
  def parserPorts : Parser[List[elemPort]] ={
    "("~repsep(parserOnePort,",")~")" ^^ { x => x match {
      case c~ports~c2 => ports
      case _ => List[elemPort]() 
      }
    }
  }

  def parserOnePort : Parser[elemPort] = {
    (parserInDecl|parserOutDecl)~ident ^^ { x => x match {
        case elemInPort(signalType)~ident => elemPort(ident,signalType,0)
        case elemOutPort(signalType)~ident => elemPort(ident,signalType,1)
        case _ => null
      }
    }
  }
  def parserInDecl : Parser[elemInPort] = "input"~parserSignalType ^^ { case in~signalType => elemInPort(signalType) }
  def parserOutDecl : Parser[elemOutPort] = "output"~parserSignalType ^^ { case out~signalType => elemOutPort(signalType) }
  def parserSignalType : Parser[String] = {
    opt("reg")~opt(parserRange) ^^ { x => x match {
        case None~None => "Bool"
        case Some("reg")~None => "Reg(Bool)"
        case None~Some(range) => "Bits( " + range + " bits)"
        case Some("reg")~Some(range) => "Reg(Bits( " + range + " bits))"
        case _ => null
      }
    }
  }
  def parserRange : Parser[String] = {
    "["~parserExpr~":"~parserExpr~"]" ^^ { case br~high~m~low~br2 => "( " + high + " - " + low + " + 1 )"}
  }
  def parserExpr : Parser[String] = {
    (ident|parserInt|ident~"-"~parserInt) 
  }
  def parserInt : Parser[String] = {
    numericLit ^^ { x => x.toString }
  }
  def digit : Parser[Any] = {
    numericLit
  }
  // def parserBits : Parser[String] = {
  //   numericLit~"'h'"~stringLit ^^ { case n~d~s => n.toString + "'" + s }
  // }
  def parserAll[T]( p : Parser[T], input :String) = {
    phrase(p)( new lexical.Scanner(input))
  }
}


