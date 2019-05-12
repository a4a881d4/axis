package open5g.lib.common
import scala.collection.mutable.ArrayBuffer

case class SignalFormat() {
  var items = ArrayBuffer[(String,Int)]()
  def toMap : Map[String,Int] = items.toMap
  def add(name:String,len:Int) : Unit = {
    items += ((name,len))
  }
  def TabHead : String = {
    var r = ""
    for((n,l) <- items) {
      val temp = "       " + n
      r += temp.substring(temp.length-l+1) +"|"
    }
    r
  }
  def Tab(a:Map[String,Int]) : String = {
    var r =""
    for((n,l) <- items) {
      val temp = "           " + (if(a.contains(n)) a(n).toString else "")
      r += temp.substring(temp.length-l+1) +"|"
    }
    r
  }
}