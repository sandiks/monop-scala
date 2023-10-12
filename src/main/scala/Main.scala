import monoplib.Auction
import monoplib.*
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

@main def startGame: Unit = {
  println("start game!")
  var playerName = "Bobby"

  var pl1 = Player(1, "Bobby", false)
  var pl2 = Player(2, "John", false)
  var g = GameHelper.startGame(List(pl1, pl2))
  var cmd = "start"
  for(line <- g.Cells.map(c=> c.toString()))
    println(line)

  while (cmd != "q") {
    // MapPrinter.PrintGameInfo2(g);

    var roundText = GameHelper.showGameState(g, g.currPlayer.name)
    if (!g.currPlayer.isBot)
      println(roundText)

    cmd = readLine()

    if (GameHelper.isValidCommand(g, cmd)) {
      GameHelper.processCommand(g, cmd, playerName);
    }

  }
}
