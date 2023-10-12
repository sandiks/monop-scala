package monoplib.managers

import monoplib.Game
import monoplib.GameState
import scala.math._

object PlayerStepsManager {

    def MakeStep(g: Game):Unit =
    {
        val rand = new scala.util.Random
        if (g.State != GameState.BeginStep) return

        if (g.currPlayer.isBot &&
            GameManager.botActionsBeforeRoll(g))
            return

        g.currPlayer.updateTimer()
        var r1: Int = 0
        var r2: Int = 0

        if (!g.Config.isManualRollMode)
        {
            r1 = 1 + rand.nextInt(6)
            r2 = 1 + rand.nextInt(6)
            g.LastRollAsInt = r1 * 10 + r2
            if (r1 == r2)
                g.currPlayer.doubleRoll += 1
            else
                g.currPlayer.doubleRoll = 0
        }
        else
        {
            g.Players.foreach(pl =>
               {
                   if (pl.isBot) pl.manualRoll = rand.nextInt(6) + 1;
               });
            var sum = 0
            g.Players.foreach(pl => { if (pl.id != g.currPlayer.id) sum += pl.manualRoll})
            r1 = g.currPlayer.manualRoll
            r2 = if sum != 0 then floor(sum.toDouble / (g.Players.length - 1)).toInt else rand.nextInt(6) + 1
        }
        g.methodsTrace += s"[MakeStep] rolled $g.LastRollAsInt"
        makeStepRoll(g);
        // g.LogRounds.Add(new() { Round = g.Round, Pid = g.Selected, Roll = g.LastRoll });
    }

    def makeStepRoll(g: Game):Unit = {

    }

    def moveAfterRandom(g: Game):Unit = {

    }
}
