package monoplib.ailogic

import monoplib.Game
import monoplib.Player
import monoplib.TradeBox
import monoplib.GameState

object TradeLogic {
  def tryDoTrade(g: Game): Boolean = {
    var validBotTrades = getValidTrades(g, g.currPlayer)
    var found = validBotTrades.find(trade => {
        g.RejectedTrades.exists(rejectedTrade => rejectedTrade == trade)!=true
    })

    if (found.isDefined) {
      g.currTradeBox = found.get
      g.State = GameState.Trade
      return true
    }
    return false
  }

  def getValidTrades(g: Game, pl: Player): List[TradeBox] = {
    List()
  }
}
