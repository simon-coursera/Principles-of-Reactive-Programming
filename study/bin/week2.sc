object week2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  Some(1,2,3)                                     //> res0: Some[(Int, Int, Int)] = Some((1,2,3))
  
  case class Event(time:Int)
  
  val e = Event(2)                                //> e  : week2.Event = Event(2)
}