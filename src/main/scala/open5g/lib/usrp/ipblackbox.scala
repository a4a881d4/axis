package open5g.lib.usrp

import spinal.core._
import spinal.lib._

class IBFGDS extends BlackBox {
  val io = new Bundle {
    val I = in Bool
    val IB = in Bool
    val O = out Bool
  }
  val debug = new Bundle {
    val I = in Bool
  }
  noIoPrefix()
}

import scala.util.parsing.combinator.syntactical._

trait ParserElem
class elemExpr extends ParserElem
case class elemIdent(name:String) extends ParserElem

object elemModule {
  def exists(Signals:List[String],l:List[elemPort]) = Signals.forall(x => l.exists(y => (y.name.indexOf(x) != -1)))
  val AxisSignal  = List("tready","tvalid","tdata","tlast")
  def AxisUSignal = ("tuser" :: AxisSignal)
  
}
case class elemModule(name:String,generics:List[elemGeneric],ports:List[elemPort]) extends ParserElem {
  def portPerfix = {
    ports.groupBy{x => 
      val i = x.name.lastIndexOf("_")
      if(i == -1) x.name else x.name.take(i)
    }
  }

  def toAxis(k:String,l:List[elemPort]) = {
    val axisu = elemModule.exists(elemModule.AxisUSignal,l)
    val axis  = elemModule.exists(elemModule.AxisSignal,l)
    if(axis) {
      var  r = "\t\t"
      r += s"val $k = "
      val tdata = l.filter(x => (x.name.indexOf("tdata") != -1))(0)
      r += (if(tdata.dir == 0) "slave " else "master ")
      r += s"Stream(Bundle{val data = ${tdata.signal};val last = Bool"
      if(axisu) {
        val tuser = l.filter(x => (x.name.indexOf("tuser") != -1))(0)
        r += s"; val user = ${tuser.signal}"
      }
      r += "})"
      List(r)
    } else {
      l.map(_.toString)
    }
  }
  def portGroup = {
    val gp = portPerfix
    gp.map{case (k,l) => toAxis(k,l)}.reduce(_ ++ _)
  }
  def toGenericString = if(generics.length > 0) generics.map(_.toString).reduce(_ + ",\n" + _) else ""
  override def toString = {
    "case class " + name + "(" + toGenericString + ")" + " extends BlackBox {\n" +
    "\tval io = new Bundle {\n" + portGroup.reduce(_ + "\n" + _) + "\n" +
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
  def remove(k:String) = elemPort(name.drop(k.length+1),signal,dir)
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

  def expr   : Parser[Any] = term ~ rep(addsub) ^^ { x => x match {
      case a~List() => a
      case a~ls => a + ls.reduce(_+_)
      case _ => ""   
    }
  }
  def term   : Parser[Any] = factor ~ rep(muldiv) ^^ { x => x match {
      case a~List() => a
      case a~ls => a + ls.reduce(_+_)
      case _ => ""   
    }
  }
  def addsub : Parser[String] = {
    ("+"~factor|"-"~factor) ^^ { case a~b => a.toString+b }
  }
  def muldiv : Parser[String] = {
    ("*"~factor|"/"~factor) ^^ { case a~b => a.toString+b }
  }
  def factor : Parser[String] = {
      (numericLit | ident | "("~expr~")") ^^ { _.toString }
  }

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
    "parameter"~ident~"="~parserExpr ^^ { case parameter~ident~e~value => elemGeneric(ident,value.toString) }
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
        case None~Some(range) => "Bits(" + range + " bits)"
        case Some("reg")~Some(range) => "Reg(Bits(" + range + " bits))"
        case _ => null
      }
    }
  }
  def parserRange : Parser[String] = {
    "["~parserExpr~":"~parserExpr~"]" ^^ { case br~high~m~low~br2 => rangeString(high.toString,low.toString)}
  }
  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }
  def rangeString(high:String,low:String) = {
    (toInt(high),toInt(low)) match {
      case (Some(h),Some(l)) => (h-l+1).toString
      case _ => {
        val h_1 = high.indexOf("-1")
        (if(h_1 == -1) high else high.take(h_1)) + 
        (if(low != "0") " - "+low+" " else "") +
        (if(h_1 == -1) "+1" else "")  
      }
    }
  }
  def parserExpr : Parser[Any] = {
    expr 
  }
  def parserInt : Parser[String] = {
    numericLit ^^ { x => x.toString }
  }
  def digit : Parser[Any] = {
    numericLit
  }
  def parserAll[T]( p : Parser[T], input :String) = {
    phrase(p)( new lexical.Scanner(input))
  }
  
  def findBrackets(s:String) = {
    var m = 0
    while(s.charAt(m) != '(' && m < s.length) {
      m += 1
    }
    if(m != s.length) {
      (s.take(m),s.drop(m))
    } else {
      ("",s)
    }
  }
  
  def findMatch(s:String) = {
    var m = 0
    var i = 0
    if(s.charAt(i) == '(') {
      m += 1
      i += 1
    }
    while(m!=0 && i < s.length) {
      if(s.charAt(i) == '(') m += 1
      if(s.charAt(i) == ')') m -= 1
      i += 1
    }
    (s.take(i),s.drop(i))
  }

  def removeComment(s:Iterator[String]) = {
    s.map{x => {
        val i = x.indexOf("//")
        if(i == -1) x else x.take(i)
      }
    }
  }

  def getModule(s:String) = {
    val (f,e) = findBrackets(s)
    if(f.indexOf("#") == -1) {
      val (e0,e1) = findMatch(e)
      f+e0+";\n"
    } else {
      val (e0,e1) = findMatch(e)
      val (e2,e3) = findMatch(e1)
      f+e0+e2+";\n"
    }
  }
}


