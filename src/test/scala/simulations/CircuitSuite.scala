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

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate") {
    def testOr(f: (Wire, Wire, Wire) => Unit) {
      val in1, in2, out = new Wire
      f(in1, in2, out)
      in1.setSignal(false)
      in2.setSignal(false)
      run
      assert(out.getSignal === false, "or 1")

      in1.setSignal(true)
      run
      assert(out.getSignal === true, "or 2")

      in2.setSignal(true)
      run
      
      assert(out.getSignal === true, "or 3")
    }
    testOr(orGate)
    testOr(orGate2)
  }  

  test("demux") {
    val in = new Wire
    val out = List(new Wire)
    demux(in, Nil, out)
    in.setSignal(false)
    run
    assert(out(0).getSignal === false, "empty control 1")
    in.setSignal(true)
    run
    assert(out(0).getSignal === true, "empty control 2")

    val in2 = new Wire 
    val ctrl = List(new Wire)
    val out2 = List(new Wire, new Wire)
    demux(in2,ctrl,out2)
    in2.setSignal(true)
    ctrl(0).setSignal(true)
    run
    assert(out2(0).getSignal === false, "one ctrl on 1")    
    assert(out2(1).getSignal === true, "one ctrl on 2")    
    ctrl(0).setSignal(false)
    run
    assert(out2(0).getSignal === true, "one ctrl on 1")    
    assert(out2(1).getSignal === false, "one ctrl on 2")

    val in3 = new Wire 
    val ctrl3 = (0 until 3).map(i => new Wire).toList
    val out3 = (0 until 8).map(i => new Wire).toList
    demux(in3,ctrl3,out3)
    in3.setSignal(true)
    ctrl3(2).setSignal(true)
    run
    assert(out3(3).getSignal === true, "one ctrl on 3")    
    ctrl3(1).setSignal(true)
    ctrl3(0).setSignal(true)
    run
    assert(out3(7).getSignal === true, "one ctrl on 3")    
  }  
}
