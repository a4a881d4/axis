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
case class elemModule(name:String,generics:List[elemGeneric],ports:List[elemPort]) extends ParserElem
case class elemGeneric(name:String,value:String) extends ParserElem
case class elemPort(name:String,signal:String,dir:Int) extends ParserElem

class verilogParser extends StandardTokenParsers {
  lexical.delimiters += (",","#","(",")",";","=","[","]",":")
  lexical.reserved   += ("module",
    "parameter",
    "input",
    "output",
    "wire",
    "reg")

  def parserModule : Parser[Any] = {
    "module"~ident~opt(parserGenerics)~parserPorts~";" ^^ { x => x match {
        case module~ident~Some(gen)~ports~com => elemModule(ident,gen,List[elemPort]())
        case module~ident~None~ports~com      => elemModule(ident,List[elemGeneric](),List[elemPort]())
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
  def parserPorts : Parser[Any] ={
    "("~repsep(parserOnePort,",")~")" ^^ { x => x match {
      case c~ports~c2 => ports
      case _ => List[elemPort]() 
      }
    }
  }
  def parserOnePort : Parser[Any] = (parserInDecl|parserOutDecl)~ident
  def parserInDecl : Parser[Any] = "input"~parserSignalType
  def parserOutDecl : Parser[Any] = "output"~parserSignalType
  def parserSignalType : Parser[Any] = ("wire"~opt(parserRange)) | ("reg"~opt(parserRange))
  def parserRange : Parser[Any] = {
    "["~parserExpr~":"~parserExpr~"]"
  }
  def parserExpr : Parser[String] = {
    (ident|parserInt)
  }
  def parserInt : Parser[String] = {
    numericLit ^^ { x => x.toString }
  }
  // def parserBits : Parser[Any] = {
  //   parserInt~"'"~"d"~parserInt | parserInt~"'"~"h"~parserHex
  // }
  def digit : Parser[Any] = {
    numericLit
  }
  def parserHex : Parser[Any] = {
    numericLit
  }
  def parserAll[T]( p : Parser[T], input :String) = {
    phrase(p)( new lexical.Scanner(input))
  }
}


