package monoplib

import scala.collection.mutable.ListBuffer

class Auction(
    currBid: Int,
    currPlayerId: Int,
    lastBiddedPlayerId: Int,
    finished: Boolean,
    aucPlayers: ListBuffer[Player] = ListBuffer()
) {
  def nextBid(bidAmount: Int) = currBid + bidAmount
  def initPlayers(players: List[Player]) = aucPlayers.addAll(players)

  override def toString() =
    s"curr: $currPlayerId, bid: $currBid, last_player: $lastBiddedPlayerId, finished: $finished"
}

class ARule {
  var id: Int = 0
  var disabled: Boolean = false
  var groupId: Int = 0
  var myCount: Int = 0
  var anCount: Int = 0
  var needBuildHouses: Boolean = false
  var housesGroups: String = ""
  var myMoney: Int = 0
  var factor: Double = 0.0
}

class ChestCard(
    var randomGroup: Int = 0,
    var cardType: Int = 0,
    var text: String = "",
    var money: Int = 0,
    var pos: Int = 0
){
  override def toString() =
    s"type: $cardType, randGroup: $randomGroup, text: $text, pos: $pos"
}

class GameConfig(
    var isManualRollMode: Boolean = false,
    var needShowLog: Boolean = false,
    var uiShowOkWhenEndround: Boolean = false,
    var isConsole: Boolean = false,
    var updateInterval: Int = 1000
) {
  def this(showLog: Boolean, manualRollMode: Boolean) =
    this(manualRollMode, showLog, false, false, 1000)
}

class RoundLogItem {
  var round: Int = 0
  var pid: Int = 0
  var roll: Int = 0
  var message: String = ""

  override def toString() =
    s"round: $round, message$message"
}

class TRule(
    var id: Int = 0,
    var getLand: Int = 0,
    var getCount: Int = 0,
    var myCount: Int = 0,
    var getMoney: Int = 0,
    var yourCount: Int = 0,
    var giveLand: Int = 0,
    var giveCount: Int = 0,
    var giveMoney: Int = 0,
    var disabled: Boolean = false,
    var moneyFactor: Double = 0.0
)

class TradeBox {
  var id: Int = 0
  var reversed: Boolean = false
  var from: Player = null
  var to: Player = null
  var giveCells: List[Int] = null
  var getCells: List[Int] = null
  var giveMoney: Int = 0
  var getMoney: Int = 0

  def equals(an: TradeBox): Boolean = {
    return true
  }

}
enum GameState {
  case Start,
    BeginStep,
    CanBuy,
    Auction,
    Trade,
    NeedPay,
    CantPay,
    EndStep,
    MoveToCell,
    RandomCell,
    FinishGame,
}
