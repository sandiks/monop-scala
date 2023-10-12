package monoplib.managers

import monoplib.ailogic.CellsLogic
import monoplib.Game
import monoplib.ailogic.TradeLogic
import monoplib.ailogic.HousesLogic

object GameManager {
    def botActionsBeforeRoll(g: Game):Boolean =
    {
        if (TradeLogic.tryDoTrade(g))
            TradeManager.runTradeJob(g)
        return false
    }

    def botActionsWhenFinishStep(g: Game) =
    {
        CellsLogic.unMortgageSell(g);
        var sum = 0.8 * g.calcPlayerAssets(g.currPlayer.id, false);
        HousesLogic.buildHouses(g, sum.toInt);
    }
}
