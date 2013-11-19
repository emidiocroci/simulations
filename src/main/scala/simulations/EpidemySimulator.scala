package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def randomBetween(min: Int, max: Int) = (random * (max - 1)).toInt + 1

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8  

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val maxMoveTime = 5
    val minWaitToMove = 1
    val maxWaitToMove = 5
  }

  import SimConfig._

  val persons: List[Person] = {
    (0 until population).map(i => {
      val p = new Person(i)
      val toInfect = population * prevalenceRate
      if(i < toInfect)
        p.infected = true
      p
      }).toList
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    var neighboursRooms = List(List(1,0),List(0,1),List(-1,0),List(0,-1))

    def waitToMove() {
      randomBetween(minWaitToMove, maxWaitToMove)
    }

    def availableRooms() {
      neighboursRooms.map(r => List(r(0) + row, r(1) + col)).filter(cell => persons.count(p => p.dead || p.sick) == 0)
    }

    def move() {
      val length = availableRooms.length
      if(length > 0)
        availableRooms()(randomBelow(length))
    }

    def completeMove() {
      afterDelay(randomBelow(maxMoveTime)) { move }
    }

    def startMove() {
      if(!dead){
        afterDelay(waitToMove) { completeMove }
      }
    }
  }

  def move(p: Person) = {

  }
}
