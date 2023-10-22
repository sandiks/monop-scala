import scala.io.Source
import monoplib.Game
import monoplib.Cell
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import monoplib.ChestCard

object DataUtil {

  def initCells(g: Game, data: String): Unit = {

    var cells = ArrayBuffer[Cell]()
    // getClass.getResource("/myFile.txt")

    for (line <- data.split("\\R").drop(1).filter(line => line != "")) {
        var arr = line.split('|')
        var cell = Cell()
        cell.title = arr(0).trim()
        cell.id = Try(arr(1).trim().toInt).getOrElse(0)
        cell.cost = Try(arr(2).trim().toInt).getOrElse(0)
        cell.ctype = Try(arr(3).trim().toInt).getOrElse(0)
        cell.group = Try(arr(4).trim().toInt).getOrElse(0)
        cell.rentInfo = if arr.length>5 then arr(5).trim() else ""
        cell.info = if arr.length>6 then arr(6).trim() else ""
        cells += cell
    }
    g.Cells = cells.sortBy(_.id)
  }

  def initChestCards(g: Game, data: String): Unit = {
    var cards = ArrayBuffer[ChestCard]()

    for (line <- data.split("\\R").drop(1).filter(line => line != "")) {
        var arr = line.split('|')
        var card = ChestCard()
        card.randomGroup = Try(arr(0).trim().toInt).getOrElse(0)
        card.cardType = Try(arr(1).trim().toInt).getOrElse(0)
        card.text = arr(2).trim()
        card.money = if arr.length > 3 then Try(arr(3).trim().toInt).getOrElse(0) else 0
        card.pos = if arr.length > 4 then Try(arr(4).trim().toInt).getOrElse(0) else 0
        cards += card
        if(card.cardType == 1)
          g.CommunityChest += card
        else
          g.ChanceChest += card

    }
    //g.CommunityChest = cards.filter(_.cardType ==1)
    //g.ChanceChest = cards.filter(_.cardType ==2)
  }
}
