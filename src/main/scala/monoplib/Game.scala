package monoplib

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import monoplib.managers.PlayerStepsManager
import monoplib.ailogic.AuctionStrategy
import monoplib.managers.PlayerManager


class Game {
    var Id: Int                             = 0
    var Round: Int                          = 0
    var LastRollAsInt: Int                  = 0
    var ManualRoll: Int                     = 0
    var Config: GameConfig                  = GameConfig(true, true)
    var State: GameState                    = GameState.Start
    var UILanguage: String                  = ""
    var Players: ArrayBuffer[Player]        = ArrayBuffer()
    var Cells: ArrayBuffer[Cell]            = ArrayBuffer()
    var Selected: Int                       = 0
    var SelectedPos: Int                    = 0
    var PlayerCellGroups: Array[Array[Int]] = Array()
    var PayAmount: Int                      = 0
    var PayToUser: Int                     = 0

    var CommunityChest: Array[ChestCard]       = Array()
    var ChanceChest: Array[ChestCard]          = Array()
    var LastRandomCard: ChestCard              = ChestCard()
    var CurrAuction: Auction                   = null
    var BotAuctionRules: ListBuffer[ARule]     = ListBuffer()
    var currTradeBox: TradeBox                 = TradeBox()
    var CompletedTrades: ListBuffer[TradeBox]  = ListBuffer()
    var RejectedTrades: ListBuffer[TradeBox]   = ListBuffer()
    var BotTradeRules: ListBuffer[TRule]       = ListBuffer()
    var auctionStrategy: AuctionStrategy       = AuctionStrategy()
    var map: Map       = new Map(this)
    val DOUBLE_ROLLS = Array(11, 22, 33, 44, 55, 66)
    // var MapHelper: monoplib.Entities.Map      = null

    def playerTradeRules(pid: Int) = BotTradeRules

    def currPlayer = Players(Selected)

    def currCell = Cells(currPlayer.pos)

    def findPlayerBy(pid: Int) = Players.find(p=> p.id == pid)
    def lastRoll = (LastRollAsInt / 10, LastRollAsInt % 10)

    var labelsLog: ListBuffer[String] = ListBuffer()
    var roundMessages: ListBuffer[String] = ListBuffer()
    var methodsTrace: ListBuffer[String] = ListBuffer()

    def logRoundMessage(text: String) = println(s"$Round $text");
    def addRoundMessage(ru_text: String, en_text: String) = logRoundMessage(buildMessage(ru_text, en_text));
    def buildMessage(ru_text: String, en_text: String) = if (UILanguage == "ru")  ru_text else en_text
    def checkRollAndMakeStep() =
    {
        var needRoll = true
        if (Config.isManualRollMode)
            needRoll = Players.forall(pl => (pl.isHuman && pl.manualRoll != 0) || pl.isBot);
        if (needRoll)
            PlayerStepsManager.MakeStep(this);
    }

    def finishStep(text: String) = {}
    def finishGameRound() = {}
    def toFirstRound(updateInterval:Int = 200) = {}
    def moveToCell() = {
       if (currPlayer.isBot)
            PlayerStepsManager.moveAfterRandom(this)
        else
            State = GameState.MoveToCell
    }
    def toBeginState() = State = GameState.BeginStep

    def finishAfterChestCard() = {
       if (currPlayer.isBot)
            PlayerStepsManager.moveAfterRandom(this)
        else
            State = GameState.MoveToCell
    }

    def toPay(finishStep:Boolean = true) = {
        methodsTrace.addOne("[ToPay]")
        State = GameState.NeedPay
        if (currPlayer.isBot)
           PlayerManager.pay(this, finishStep)
    }
    def toPayAmount(amount:Int, finishStep:Boolean = true) = {
        methodsTrace.addOne(s"[ToPayAmount] amount:${amount}")
        PayAmount = amount
        toPay(finishStep)
    }
    def ToCanBuy() = {
        methodsTrace.addOne("[ToCanBuy]")
        State = GameState.CanBuy
    }
    def toCantPay() = {
        methodsTrace.addOne("[toCantPay]")
    }
    def toAuction() = {
        State = GameState.Auction
        auctionStrategy.initAuction()
    }

    def calcPlayerAssets(pid: Int, includeMonop: Boolean = true):Int =
    {
        var sum = 0
        Cells.filter(c => c.IsActive && c.owner == pid).foreach(cell=>
        {
            if (includeMonop)
            {
                sum += cell.mortgageAmount
                sum += cell.housesCount * cell.HouseCostWhenSell
            } else if (!cell.isMonopoly){
                sum += cell.mortgageAmount
            }
        })
        sum += findPlayerBy(pid).get.money
        return sum
    }

    def playerLeaveGame() =
    {
        var pl = currPlayer
        if (Players.length >= 2)
        {
            Players -= pl
            for (cell <- map.CellsByUser(pl.id))
            {
                cell.owner = -1
                cell.housesCount = 0
            }
            PlayerCellGroups(pl.id) = Array()
            addRoundMessage(s"${pl.name} покинул игру", s"${pl.name} left game");
        }

    }
}
