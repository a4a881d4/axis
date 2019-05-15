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

class verilogParser extends StandardTokenParsers {
  lexical.delimiters += (",","#","(",")",";","=","[","]",":")
  lexical.reserved   += ("module",
    "parameter",
    "input",
    "output",
    "wire",
    "reg")

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
    "("~repsep(parserOnePort,",")~")"
  }
  def parserOnePort : Parser[Any] = (parserInDecl|parserOutDecl)~ident
  def parserInDecl : Parser[Any] = "input"~parserSignalType
  def parserOutDecl : Parser[Any] = "output"~parserSignalType
  def parserSignalType : Parser[Any] = ("wire"~opt(parserRange)) | ("reg"~opt(parserRange))
  def parserRange : Parser[Any] = {
    "["~parserExpr~":"~parserExpr~"]"
  }
  def parserExpr : Parser[Any] = {
    (ident|parserInt)
  }
  def parserInt : Parser[Any] = {
    numericLit
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


