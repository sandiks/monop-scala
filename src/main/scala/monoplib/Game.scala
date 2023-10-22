package monoplib

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import monoplib.managers.PlayerStepsManager
import monoplib.ailogic.AuctionStrategy
import monoplib.managers.PlayerManager
import monoplib.managers.GameManager

object Game {
    val DOUBLE_ROLLS = Array(11, 22, 33, 44, 55, 66)
}

class Game {
    var Id: Int                             = 0
    var round: Int                          = 0
    var lastRollAsInt: Int                  = 0
    var ManualRoll: Int                     = 0
    var config: GameConfig                  = GameConfig(true, false)
    var State: GameState                    = GameState.Start
    var UILanguage: String                  = "ru"
    var Players: ArrayBuffer[Player]        = ArrayBuffer()
    var Cells: ArrayBuffer[Cell]            = ArrayBuffer()
    var selected: Int                       = 0
    var SelectedPos: Int                    = 0
    var PlayerCellGroups: Array[Array[Int]] = Array()
    var payAmount: Int                      = 0
    var payToUser: Int                     = 0

    var CommunityChest: ArrayBuffer[ChestCard]       = ArrayBuffer()
    var ChanceChest: ArrayBuffer[ChestCard]          = ArrayBuffer()
    var lastRandomCard: ChestCard              = ChestCard()
    var CurrAuction: Auction                   = null
    var botAuctionRules: ListBuffer[ARule]     = ListBuffer()
    var currTradeBox: TradeBox                 = TradeBox()
    var completedTrades: ListBuffer[TradeBox]  = ListBuffer()
    var rejectedTrades: ListBuffer[TradeBox]   = ListBuffer()
    var botTradeRules: ListBuffer[TRule]       = ListBuffer()
    var auctionStrategy: AuctionStrategy       = AuctionStrategy()
    var map: Map       = new Map(this)
    // var MapHelper: monoplib.Entities.Map      = null

    def playerTradeRules(pid: Int) = botTradeRules

    def currPlayer = Players(selected)

    def currCell = Cells(currPlayer.pos)

    def findPlayerBy(pid: Int) = Players.find(p=> p.id == pid)
    def lastRoll = (lastRollAsInt / 10, lastRollAsInt % 10)

    var labelsLog: ListBuffer[String] = ListBuffer()
    var roundMessages: ListBuffer[String] = ListBuffer()
    var methodsTrace: ListBuffer[String] = ListBuffer()

    def logx(text: String) = labelsLog.addOne(text)
    def addRoundMessage(ru_text: String, en_text: String) = logRoundMessage(buildMessage(ru_text, en_text))
    def addRoundMessage(text: String) = logRoundMessage(text)
    def addRoundMessageByLabel(label:String, args: String*) =
        logRoundMessage(GameTexts.get(UILanguage, label).format(args))

    def buildMessage(ru_text: String, en_text: String) = if (UILanguage == "ru")  ru_text else en_text
    def logRoundMessage(text: String) = println(s"$round $text")

    def checkRollAndMakeStep() =
    {
        var needRoll = true
        if (config.isManualRollMode)
            needRoll = Players.forall(pl => (pl.isHuman && pl.manualRoll != 0) || pl.isBot)
        if (needRoll)
            PlayerStepsManager.makeStep(this);
    }

    def finishStep(text: String): Unit = {}

    def finishGameRound(): Unit = {
        if (State != GameState.EndStep) return
        roundMessages.clear()
        methodsTrace.clear()

        if (currPlayer.isBot)
            GameManager.botActionsWhenFinishStep(this)
        logx("_round_finished")
        println("--------------------")

        if (config.isManualRollMode)
            Players.foreach(pl => pl.manualRoll = 0)
        round +=1
        if(!Game.DOUBLE_ROLLS.contains(lastRollAsInt)) selected+=1
        if (selected >= Players.length) selected = 0
        State = GameState.BeginStep
        currPlayer.updateTimer()
        if (currPlayer.isBot) checkRollAndMakeStep()
    }

    def toFirstRound(updateInterval:Int = 200) = {
        State = GameState.BeginStep
        round = 1
        config.updateInterval = updateInterval
    }

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
        payAmount = amount
        toPay(finishStep)
    }
    def toCanBuy() = {
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
        Cells.filter(c => c.isActive && c.owner == pid).foreach(cell=>
        {
            if (includeMonop)
            {
                sum += cell.mortgageAmount
                sum += cell.housesCount * cell.houseCostWhenSell()
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
