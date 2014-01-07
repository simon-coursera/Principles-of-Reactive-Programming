package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  object Helper {
    def getElements(h:H): List[A] = {
    	if(isEmpty(h)) Nil 
    	else  findMin(h) :: getElements(deleteMin(h))
    }
  }
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("2element") = forAll{ (m1:A, m2:A) =>
    findMin(insert(m1,insert(m2, empty))) == {if(m1 < m2) m1 else m2}
  }
  
  property("insert_delete_empty") = forAll{ (m:A) =>
    deleteMin(insert(m,empty)) == empty
  }
  
  property("sorted") = forAll{ (h:H) => 
    val elements:List[A] = Helper.getElements(h)

    elements == elements.sorted
  }
  
  property("minimum_of_2") = forAll{ (h1:H, h2:H) =>
    val smallest = {if(findMin(h1) < findMin(h2)) findMin(h1) else findMin(h2)}
    
    smallest == findMin(meld(h1, h2))
  }
  
  property("No_Missing") = forAll { (l: List[A]) =>
  	def insertAll(elements: List[A]): H = {
	  if(elements.isEmpty) empty
	  else insert(elements.head, insertAll(elements.tail))
  	}
  	
  	l.toSet == Helper.getElements(insertAll(l)).toSet
  }
  
  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <-  Gen.frequency((1, value(empty)), (10, genHeap))
    //h <-  oneOf(value(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
