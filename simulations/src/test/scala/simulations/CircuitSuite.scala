package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("demux example") {
    val in = new Wire
    val c = List(new Wire, new Wire, new Wire)
    val out = List(new Wire, new Wire, new Wire, new Wire,
    				new Wire, new Wire, new Wire, new Wire)
    
    demux(in, c, out)
    in.setSignal(true)
    
    run
    assert(out(7).getSignal === true , "and 1.1 ")
    assert(out(0).getSignal === false , "and 1.4")

    c(0).setSignal(false)
    c(1).setSignal(true)
    c(2).setSignal(true)
    run
    assert(out(3).getSignal === false, "and 2.1 ")
    assert(out(4).getSignal === true, "and 2.4")
    
    c(0).setSignal(true)
    c(1).setSignal(false)
    c(2).setSignal(true)
    run
    assert(out(2).getSignal === true, "and 2.1 ")
    assert(out(7).getSignal === false, "and 2.4")
    
  }
}
