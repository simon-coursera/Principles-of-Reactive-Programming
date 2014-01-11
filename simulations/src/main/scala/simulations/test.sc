package simulations

import gui._

object test {

  val es = new EpidemySimulator                   //> es  : simulations.EpidemySimulator = simulations.EpidemySimulator@73d0a8dc
  es.persons.foreach(p => {
    p.infected = false
    p.sick = false
    p.dead = false
    p.row = 50
    p.col = 50
  })

  es.persons(0).row = 0
  es.persons(0).col = 1
  es.persons(0).sick = true
 
  es.persons(1).row = 1
  es.persons(1).col = 0
  es.persons(1).dead = true

  es.persons(2).row = 2
  es.persons(2).col = 1
  es.persons(2).sick = true

  es.persons(3).row = 1
  es.persons(3).col = 2
  es.persons(3).dead = true

  es.persons(4).row = 1
  es.persons(4).col = 1

  es.persons(4).move

}
  