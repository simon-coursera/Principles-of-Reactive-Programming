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
    val transmissibilityRate = 40
    val diePosibility = 25
    val moveInterval = 5
    val turnSickInterval = 6
    val turnDeadInterval = 14
    val turnImmuneInterval = 16
    val turnHealthyInterval = 18
  }

  import SimConfig._

  // to complete: construct list of persons
  val persons: List[Person] = (for (id <- 0 until population) yield new Person(id)).toList
  persons.take((prevalenceRate * population).toInt).foreach(person => person.turnInfected) //Setup prevalance infection

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
    afterDelay(randomBelow(moveInterval))(move)

    def move: Unit = {
      if (!dead) {
        val neighborRoomLeftCol = { if (col - 1 < 0) roomColumns - 1 else col - 1 }
        val neighborRoomRightCol = { if (col + 1 >= roomColumns) 0 else col + 1 }
        val neighborRoomUpRow = { if (row - 1 < 0) roomRows - 1 else row - 1 }
        val neighborRoomBottomRow = { if (row + 1 >= roomRows) 0 else row + 1 }

        val neighborRooms = List((row, neighborRoomLeftCol), (row, neighborRoomRightCol), (neighborRoomUpRow, col), (neighborRoomBottomRow, col)).
          filter(room => personsInRoom(room._1, room._2)(person => !person.sick && !person.dead).isEmpty)

        if (!neighborRooms.isEmpty) {
          val newRoom = neighborRooms(randomBelow(neighborRooms.length))

          row = newRoom._1
          col = newRoom._2
          //This person can infect the people in room or may get infected by the room
          if(infected) infectPeopleInRoom
          else if (!personsInRoom(row, col)(_.infected).isEmpty && randomBelow(100) < transmissibilityRate) turnInfected
        }

        afterDelay(randomBelow(moveInterval))(move)
      }
    }

    def infectPeopleInRoom = {
      val persons = personsInRoom(row, col)(person => !person.infected).filter(person => randomBelow(100) < transmissibilityRate)
      persons.foreach(person => person.turnInfected)
    }

    def turnInfected: Unit = {
      infected = true
      afterDelay(turnSickInterval)(turnSick)
    }

    def turnSick: Unit = {
      sick = true
      if (randomBelow(100) < diePosibility) afterDelay(turnDeadInterval - turnSickInterval)(turnDead)
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
      dead = true
    }
  }
}
