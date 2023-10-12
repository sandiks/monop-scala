import scala.io.Source
import monoplib.Game
import monoplib.Cell
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object DataUtil {

  def initCells(g: Game, data: String): Unit = {

    var cells = ListBuffer[Cell]()
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
    g.Cells = cells.sortBy(_.id).to(ArrayBuffer)
  }
  def InitChestCards(g: Game, folderPath: String): Unit = {}
}
