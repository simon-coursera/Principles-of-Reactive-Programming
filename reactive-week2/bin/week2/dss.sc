package week2

object dss {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  object sim extends Circuits with Parameters
  import sim._
  val in1, in2, sum, carry = new Wire             //> in1  : week2.dss.sim.Wire = week2.Gates$Wire@17d902d3
                                                  //| in2  : week2.dss.sim.Wire = week2.Gates$Wire@284876e0
                                                  //| sum  : week2.dss.sim.Wire = week2.Gates$Wire@2d7480c9
                                                  //| carry  : week2.dss.sim.Wire = week2.Gates$Wire@3bb3236f
  probe("sum", sum)                               //> sum 0 new-value = false
  probe("carry", carry)                           //> carry 0 new-value = false
  halfAdder(in1, in2, sum, carry)
  in1 setSignal true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| sum 8 new-value = true
  in2 setSignal true
  
  run()                                           //> *** simulation started, time = 8 ***
                                                  //| carry 11 new-value = true
                                                  //| sum 16 new-value = false
  in1 setSignal false
  run()                                           //> *** simulation started, time = 16 ***
                                                  //| carry 19 new-value = false
                                                  //| sum 24 new-value = true
}