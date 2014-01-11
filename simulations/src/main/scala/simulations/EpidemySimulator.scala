package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val diePosibility = 0.25
    val moveInterval = 5
    val turnSickInterval = 6
    val turnDeadInterval = 14
    val turnImmuneInterval = 16
    val turnHealthyInterval = 18
  }

  import SimConfig._

  // to complete: construct list of persons
  val persons: List[Person] = (for (id <- 0 until population) yield new Person(id)).toList

  persons.take((prevalenceRate * population).toInt).foreach(person => {
    person.infected = true
    afterDelay(0)(person.turnInfected);
  })
  persons.foreach(person => afterDelay(randomBelow(moveInterval))(person.move))

  def personsInRoom(row: Int, col: Int)(f: Person => Boolean): List[Person] =
    for (person <- persons if person.row == row && person.col == col && f(person)) yield person

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    def move: Unit = {
      if (!dead) {
        val neighborRoomLeftCol = { if (col == 0) roomColumns - 1 else col - 1 }
        val neighborRoomRightCol = { if (col == roomColumns - 1) 0 else col + 1 }
        val neighborRoomUpRow = { if (row == 0) roomRows - 1 else row - 1 }
        val neighborRoomBottomRow = { if (row == roomRows - 1) 0 else row + 1 }

        val neighborRooms = List((row, neighborRoomLeftCol), (row, neighborRoomRightCol), (neighborRoomUpRow, col), (neighborRoomBottomRow, col)).
          filter(room => personsInRoom(room._1, room._2)(person => person.sick || person.dead).isEmpty)

        if (!neighborRooms.isEmpty) {
          val newRoom = neighborRooms(randomBelow(neighborRooms.length))

          row = newRoom._1
          col = newRoom._2
          //This person can infect the people in room or may get infected by the room
          if (infected) infectPeopleInRoom
          else if (!personsInRoom(row, col)(_.infected).isEmpty && random < transmissibilityRate) turnInfected
        } else {
          //Stay in the same room could also get infected
          if (!infected && !personsInRoom(row, col)(_.infected).isEmpty && random < transmissibilityRate) turnInfected
        }

        afterDelay(randomBelow(moveInterval))(move)
      }
    }

    def infectPeopleInRoom = personsInRoom(row, col)(!_.infected).foreach(person => {
      if (random < transmissibilityRate) person.turnInfected
    })

    def turnInfected: Unit = {
      if (!dead) {
        infected = true
        afterDelay(turnSickInterval)(turnSick)
      }
    }

    def turnSick: Unit = {
      sick = true
      if (random < diePosibility) afterDelay(turnDeadInterval - turnSickInterval)(turnDead)
      else afterDelay(turnImmuneInterval - turnSickInterval)(turnImmune)
    }

    def turnImmune: Unit = {
      sick = false
      immune = true
      afterDelay(turnHealthyInterval - turnImmuneInterval)(turnHealthy)
    }

    def turnHealthy: Unit = {
      infected = false
      sick = false
      immune = false
    }

    def turnDead: Unit = {
      immune = false
      dead = true
    }
  }
}

//Reduce Mobility Act. The mobility of people is decreased by half. The mobility of a visibly infected person is further reduced by half.
class EpidemySimulator1 extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val diePosibility = 0.25
    val moveInterval = 10
    val turnSickInterval = 6
    val turnDeadInterval = 14
    val turnImmuneInterval = 16
    val turnHealthyInterval = 18
  }

  import SimConfig._

  // to complete: construct list of persons
  val persons: List[Person] = (for (id <- 0 until population) yield new Person(id)).toList

  persons.take((prevalenceRate * population).toInt).foreach(person => {
    person.infected = true
    afterDelay(0)(person.turnInfected);
  })
  persons.foreach(person => afterDelay(randomBelow(moveInterval))(person.move))

  def personsInRoom(row: Int, col: Int)(f: Person => Boolean): List[Person] =
    for (person <- persons if person.row == row && person.col == col && f(person)) yield person

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    def move: Unit = {
      if (!dead) {
        val neighborRoomLeftCol = { if (col == 0) roomColumns - 1 else col - 1 }
        val neighborRoomRightCol = { if (col == roomColumns - 1) 0 else col + 1 }
        val neighborRoomUpRow = { if (row == 0) roomRows - 1 else row - 1 }
        val neighborRoomBottomRow = { if (row == roomRows - 1) 0 else row + 1 }

        val neighborRooms = List((row, neighborRoomLeftCol), (row, neighborRoomRightCol), (neighborRoomUpRow, col), (neighborRoomBottomRow, col)).
          filter(room => personsInRoom(room._1, room._2)(person => person.sick || person.dead).isEmpty)

        if (!neighborRooms.isEmpty) {
          val newRoom = neighborRooms(randomBelow(neighborRooms.length))

          row = newRoom._1
          col = newRoom._2
          //This person can infect the people in room or may get infected by the room
          if (infected) infectPeopleInRoom
          else if (!personsInRoom(row, col)(_.infected).isEmpty && random < transmissibilityRate) turnInfected
        } else {
          //Stay in the same room could also get infected
          if (!infected && !personsInRoom(row, col)(_.infected).isEmpty && random < transmissibilityRate) turnInfected
        }

        if (sick) afterDelay(randomBelow(moveInterval * 2))(move)
        else afterDelay(randomBelow(moveInterval))(move)
      }
    }

    def infectPeopleInRoom = personsInRoom(row, col)(!_.infected).foreach(person => {
      if (random < transmissibilityRate) person.turnInfected
    })

    def turnInfected: Unit = {
      if (!dead) {
        infected = true
        afterDelay(turnSickInterval)(turnSick)
      }
    }

    def turnSick: Unit = {
      sick = true
      if (random < diePosibility) afterDelay(turnDeadInterval - turnSickInterval)(turnDead)
      else afterDelay(turnImmuneInterval - turnSickInterval)(turnImmune)
    }

    def turnImmune: Unit = {
      sick = false
      immune = true
      afterDelay(turnHealthyInterval - turnImmuneInterval)(turnHealthy)
    }

    def turnHealthy: Unit = {
      infected = false
      sick = false
      immune = false
    }

    def turnDead: Unit = {
      immune = false
      dead = true
    }
  }
}


//The Chosen Few Act. 5% of people (VIPs such as pop singers, football players, etc.) are given vaccines when first created. They never become infected.
class EpidemySimulator2 extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val diePosibility = 0.25
    val moveInterval = 10
    val turnSickInterval = 6
    val turnDeadInterval = 14
    val turnImmuneInterval = 16
    val turnHealthyInterval = 18
    
    val vaccinesRate = 0.05
  }

  import SimConfig._

  // to complete: construct list of persons
  val persons: List[Person] = (for (id <- 0 until population) yield new Person(id)).toList

  persons.take((prevalenceRate * population).toInt).foreach(person => {
    person.infected = true
    afterDelay(0)(person.turnInfected);
  })
  persons.takeRight((vaccinesRate * population).toInt).foreach(_.vaccined = true)
  persons.foreach(person => afterDelay(randomBelow(moveInterval))(person.move))

  def personsInRoom(row: Int, col: Int)(f: Person => Boolean): List[Person] =
    for (person <- persons if person.row == row && person.col == col && f(person)) yield person

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var vaccined = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    def move: Unit = {
      if (!dead) {
        val neighborRoomLeftCol = { if (col == 0) roomColumns - 1 else col - 1 }
        val neighborRoomRightCol = { if (col == roomColumns - 1) 0 else col + 1 }
        val neighborRoomUpRow = { if (row == 0) roomRows - 1 else row - 1 }
        val neighborRoomBottomRow = { if (row == roomRows - 1) 0 else row + 1 }

        val neighborRooms = List((row, neighborRoomLeftCol), (row, neighborRoomRightCol), (neighborRoomUpRow, col), (neighborRoomBottomRow, col)).
          filter(room => personsInRoom(room._1, room._2)(person => person.sick || person.dead).isEmpty)

        if (!neighborRooms.isEmpty) {
          val newRoom = neighborRooms(randomBelow(neighborRooms.length))

          row = newRoom._1
          col = newRoom._2
          //This person can infect the people in room or may get infected by the room
          if (infected) infectPeopleInRoom
          else if (!personsInRoom(row, col)(_.infected).isEmpty && random < transmissibilityRate) turnInfected
        } else {
          //Stay in the same room could also get infected
          if (!infected && !personsInRoom(row, col)(_.infected).isEmpty && random < transmissibilityRate) turnInfected
        }

        if (sick) afterDelay(randomBelow(moveInterval * 2))(move)
        else afterDelay(randomBelow(moveInterval))(move)
      }
    }

    def infectPeopleInRoom = personsInRoom(row, col)(!_.infected).foreach(person => {
      if (random < transmissibilityRate) person.turnInfected
    })

    def turnInfected: Unit = {
      if (!dead && !vaccined) {
        infected = true
        afterDelay(turnSickInterval)(turnSick)
      }
    }

    def turnSick: Unit = {
      sick = true
      if (random < diePosibility) afterDelay(turnDeadInterval - turnSickInterval)(turnDead)
      else afterDelay(turnImmuneInterval - turnSickInterval)(turnImmune)
    }

    def turnImmune: Unit = {
      sick = false
      immune = true
      afterDelay(turnHealthyInterval - turnImmuneInterval)(turnHealthy)
    }

    def turnHealthy: Unit = {
      infected = false
      sick = false
      immune = false
    }

    def turnDead: Unit = {
      immune = false
      dead = true
    }
  }
}