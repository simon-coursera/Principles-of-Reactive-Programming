object week2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  Some(1,2,3)                                     //> res0: Some[(Int, Int, Int)] = Some((1,2,3))
  
  case class Event(time:Int)
  
  val e = (x:Int)=>{1+x}                          //> e  : Int => Int = <function1>
	e(3)                                      //> res1: Int = 4
}