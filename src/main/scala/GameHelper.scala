import monoplib.Game
import monoplib.Player
import monoplib.GameState
import monoplib.managers.PlayerManager
import monoplib.managers.TradeManager
import monoplib.managers.PlayerStepsManager

object GameHelper {

    def startGame(players: List[Player]): Game = {
        var g = Game()
        g.Players.addAll(players)
        var landsText = scala.io.Source
            .fromFile("src/main/resources/GameData/lands_ru.txt")
            .mkString
        var randomCardsText = scala.io.Source
            .fromFile("src/main/resources/GameData/chest_cards_ru.txt")
            .mkString
        // println(landsText)
        DataUtil.initCells(g, landsText)
        DataUtil.initChestCards(g, randomCardsText)

        g.State = GameState.Start
        return g
    }

    def showGameState(g: Game, curr: String): String = {
        val infoText = g.State match {
            case GameState.Start => "gaame is start state"

            case GameState.BeginStep =>
                if g.config.isManualRollMode then
                    "start round: choose one number [1..6]"
                else "write [game roll]"
            case GameState.CanBuy =>
                s"you can buy ${g.currCell.title} or auction, write [game b] or [game a]"
            case GameState.Auction => "do you want bid? [y n]"
            case GameState.Trade =>
                s"player ${g.currTradeBox.from.id} wants trade, write [game y] or [game n]"
            case GameState.CantPay    => "you need mortgage cells to find money"
            case GameState.NeedPay    => "yoy need pay, write [p]"
            case GameState.RandomCell => "RandomCell"
            case GameState.MoveToCell => "press 'go' to proceed new position"
            case GameState.EndStep =>
                "press any key to  finish round or #p to show methods trace"
            case _ => "unknown game state"
        }
        return infoText
    }

    def isValidCommand(g: Game, cmd: String): Boolean = {
        if (
          g.State == GameState.MoveToCell || g.State == GameState.CanBuy || g.State == GameState.EndStep
        )
            return true
        if ("".equals(cmd))
            return false
        return true
    }

    def processCommand(g: Game, cmd: String, curr: String): Unit =
        {
            g.State match {
                case GameState.BeginStep => {
                    if (cmd.startsWith("m")) mortgage(g, cmd)
                    if (cmd.startsWith("um")) unMortgage(g, cmd)
                    g.checkRollAndMakeStep()
                }
                case GameState.CanBuy => {
                    if (cmd == "b" || "".equals(cmd))
                        PlayerManager.buy(g)
                    else if (cmd == "a")
                        g.toAuction()
                    else if (cmd.startsWith("m"))
                        mortgage(g, cmd)
                    else if (cmd.startsWith("um"))
                        unMortgage(g, cmd)
                }

                case GameState.Auction => g.auctionStrategy.runActionJob(cmd)

                case GameState.Trade => {
                    if (cmd != "y")
                        TradeManager.CompleteTrade(g)
                    else
                        TradeManager.AddToRejectedTrades(g)
                }

                case GameState.CantPay | GameState.NeedPay =>
                    PlayerManager.pay(g)

                case GameState.RandomCell => g.finishStep("")
                case GameState.MoveToCell =>
                    PlayerStepsManager.moveAfterRandom(g)

                case GameState.EndStep =>
                    if (cmd == "p") printMethodTrace(g) else g.finishGameRound()
                case _ => "default"
            }
        }

        def mortgage(g: Game, cmd: String) = {}
        def unMortgage(g: Game, cmd: String) = {}
        def printMethodTrace(g: Game) = {}

}
