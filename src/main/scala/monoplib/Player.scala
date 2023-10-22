package monoplib

import scala.collection.mutable.ListBuffer

enum PlayerType:
    case Bot, Human

class Player(
    var id: Int = 0,
    var name: String = "",
    var isBot: Boolean = false,
    var deleted: Boolean = false,
    var money: Int = 0,
    var pos: Int = 0,
    var lastRoll: Int = 0,
    var manualRoll: Int = 0,
    var police: Int = 0,
    var policeKey: Int = 0,
    var doubleRoll: Int = 0,
    var rolls: ListBuffer[Int] = ListBuffer()
) {

    def this(id: Int, name: String, isBot: Boolean) =
        this(id, name, isBot, false, 1500, 0, 0, 0, 0, 0, 0, ListBuffer())

    def isHuman = !isBot

    def moveToNewPosition(lastRoll: Int): Unit = {
        val r1 = lastRoll / 10
        val r2 = lastRoll % 10
        rolls += lastRoll
        if (pos > 39) {
            pos %= 40
            money += 2000
        }
    }
    def updateTimer(): Unit = {}
}
