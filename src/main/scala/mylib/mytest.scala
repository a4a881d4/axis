package mylib

import open5g.lib.zcpsm.tools._

object mytest extends asmParser {
  val psm = """
    |;; 1 
    |;; 1 unsigned char m;
    |;; 2 main()
    |;; 3 {
    |;; EXPORT _main
    |_main:
    |;; 4  register char j,k;
    |;; 5  short i,l,h;
    |;; 6  k=0;
    |LOAD  s03,  00
    |;; 7  j=85;
    |LOAD  s02,  55
    |;; 8  m=0;
    |LOAD  s1F,  00
    |;; 9  if( j==1 )
    |LOAD  s00,  s02
    |SUB s00,  01
    |JUMP  NZ, L1
    |L2:
    |;; 10   {
    |;; 11     k++;
    |ADD s03,  01
    |;; 12   }
    |;; 13   else
    |;; 14   {
    |JUMP  L3
    |L1:
    |;; 15     k--;
    |SUB s03,  01
    |;; 16   }
    |;; 17   if( j==1 )
    |L3:
    |LOAD  s00,  s02
    |SUB s00,  01
    |JUMP  NZ, L4
    |L5:
    |;; 18   {
    |;; 19     k++;
    |ADD s03,  01
    |;; 20   }
    |;; 21   else
    |;; 22   {
    |JUMP  L6
    |L4:
    |;; 23     k--;
    |SUB s03,  01
    |;; 24   }
    |;; 25 }
    |L6:
    |RETURN
    |;; 26 
    """.stripMargin
  def main(args: Array[String]) {
    val ra = reg(0)
    val ib = imm(10)
    val op = add(ra,ib)

    println(op)
    ib.data = 20
    println(op)
    
    val jp = jumpc("L1")
    println(jp)
    println(psm)

    val (a,b) = fromFile(psm)
    for(i <- 0 until a.length) {
      println(a(i)+f" ;; ${a(i).toHex}%05X")
    }
    println(b)
    println(toFile)
  }
}
