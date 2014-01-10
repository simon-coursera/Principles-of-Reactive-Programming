package simulations

object test {

  class TestSuite extends CircuitSimulator {
    val InverterDelay = 1
    val AndGateDelay = 3
    val OrGateDelay = 5
  }

  val test = new TestSuite                        //> test  : simulations.test.TestSuite = simulations.test$TestSuite@5ba07a6f
  import test._

  val in = new Wire                               //> in  : simulations.Wire = false
  val c = List(new Wire, new Wire, new Wire )     //> c  : List[simulations.Wire] = List(false, false, false)
  val out = List(new Wire, new Wire, new Wire, new Wire,
  							new Wire, new Wire, new Wire, new Wire)
                                                  //> out  : List[simulations.Wire] = List(false, false, false, false, false, fals
                                                  //| e, false, false)
	
  demux(in, c, out)
  in.setSignal(true)
  out(0) setSignal true
  out(1) setSignal true
  run                                             //> *** New propagation ***

	in                                        //> res0: simulations.Wire = true
	c                                         //> res1: List[simulations.Wire] = List(false, false, false)
	out                                       //> res2: List[simulations.Wire] = List(false, false, false, false, false, false
                                                  //| , false, true)
	
}
  