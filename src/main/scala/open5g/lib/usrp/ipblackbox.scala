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

import scala.util.parsing.combinator._

class verilogParser extends JavaTokenParsers {
  // lexical.delimiters += (",","#","(",")",";","=","[","]",":")
  // lexical.reserved   += ("module","parameter","input","output","wire")
  def parserModule : Parser[Any] = {
    "module"~ident~opt(parserGenerics)~parserPorts~";"
  }
  def parserGenerics : Parser[Any] ={
    "#"~"("~repsep(parserOneGeneric,",")~")"
  }
  def parserOneGeneric : Parser[Any] = {
    "parameter"~ident~"="~(parserExpr)
  }
  def parserPorts : Parser[Any] ={
    "("~repsep(parserOneGeneric,",")~")"
  }
  def parserOnePort : Parser[Any] = {
    ("input"|"output")~("wire"|"reg")~opt(parserRange)~ident
  }
  def parserRange : Parser[Any] = {
    "["~parserExpr~":"~parserExpr~"]"
  }
  def parserExpr : Parser[Any] = {
    (ident|parserInt|parserBits)
  }
  def parserInt : Parser[Any] = {
    digit~rep(digit)
  }
  def parserBits : Parser[Any] = {
    parserInt~("h"|"d")~parserInt
  }
  // def parserAll[T]( p : Parser[T], input :String) = {
  //   phrase(p)( new lexical.Scanner(input))
  // }
}


