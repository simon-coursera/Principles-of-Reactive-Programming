package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object test {

  val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
  }                                               //> propConcatLists  : org.scalacheck.Prop = Prop

  //propConcatLists.check
	val a = new QuickCheckHeap with Bogus3BinomialHeap
                                                  //> a  : quickcheck.QuickCheckHeap with quickcheck.Bogus3BinomialHeap = Prop
 	
 	val h1 = a.insert(-20, a.insert(10, a.insert(-110, Nil)))
                                                  //> h1  : quickcheck.test.a.H = List(Node(-20,0,List()), Node(-110,1,List(Node(-
                                                  //| 110,0,List()))))
 	val h2 = a.insert(-200, a.insert(1, a.insert(-110, Nil)))
                                                  //> h2  : quickcheck.test.a.H = List(Node(-200,0,List()), Node(-110,1,List(Node(
                                                  //| -110,0,List()))))
 	
 	
 val b = new QuickCheckHeap with BinomialHeap     //> b  : quickcheck.QuickCheckHeap with quickcheck.BinomialHeap = Prop
 val h3 = b.insert(-20, b.insert(10, b.insert(-110, Nil)))
                                                  //> h3  : quickcheck.test.b.H = List(Node(-20,0,List()), Node(-110,1,List(Node(1
                                                  //| 0,0,List()))))
 val h4 = b.insert(-200, b.insert(1, b.insert(-110, Nil)))
                                                  //> h4  : quickcheck.test.b.H = List(Node(-200,0,List()), Node(-110,1,List(Node(
                                                  //| 1,0,List()))))

}