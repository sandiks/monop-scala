package monoplib.managers

import monoplib.Game
import monoplib.GameState
import scala.math._
import monoplib.GameTexts
import scala.collection.mutable.ListBuffer
import monoplib.Player
import monoplib.Cell

object PlayerStepsManager {

    def makeStep(g: Game): Unit = {
        val rand = new scala.util.Random
        if (g.State != GameState.BeginStep) return

        if (
          g.currPlayer.isBot &&
          GameManager.botActionsBeforeRoll(g)
        )
            return

        g.currPlayer.updateTimer()
        var r1: Int = 0
        var r2: Int = 0

        if (!g.config.isManualRollMode) {
            r1 = 1 + rand.nextInt(6)
            r2 = 1 + rand.nextInt(6)
            g.lastRollAsInt = r1 * 10 + r2
            if (r1 == r2)
                g.currPlayer.doubleRoll += 1
            else
                g.currPlayer.doubleRoll = 0
        } else {
            g.Players.foreach(pl => {
                if (pl.isBot) pl.manualRoll = rand.nextInt(6) + 1
            })
            var sum = 0
            g.Players.foreach(pl => {
                if (pl.id != g.currPlayer.id) sum += pl.manualRoll
            })
            r1 = g.currPlayer.manualRoll
            r2 =
                if sum != 0 then
                    floor(sum.toDouble / (g.Players.length - 1)).toInt
                else rand.nextInt(6) + 1
        }
        g.methodsTrace += s"[MakeStep] rolled $g.LastRollAsInt"
        makeStepRoll(g)
        // g.LogRounds.Add(new() { Round = g.Round, Pid = g.Selected, Roll = g.LastRoll });
    }

    // invoke  in Player.Pay()
    def changePosAndProcessPosition(g: Game) = {
        g.methodsTrace.addOne(
          s"[ChangePosAndProcessPosition] cell:${g.currPlayer.pos}"
        )
        var pl = g.currPlayer;
        val (r1, r2) = g.lastRoll;
        pl.pos += r1 + r2;
        processPosition(g);
    }

    def makeStepRoll(g: Game): Unit = {
        var result = step(g)
        val curr = g.currPlayer
        if (result == "go") {
            var oldPos = curr.pos
            curr.moveToNewPosition(g.lastRollAsInt)
            g.addRoundMessageByLabel(
              "_you_visisted_cell",
              s"($oldPos->${curr.pos})",
              g.currCell.title
            )
            processPosition(g)
        } else {
            g.finishStep(result)
        }

    }

    def step(g: Game): String = {
        var pl = g.currPlayer
        var (r1, r2) = g.lastRoll
        var beforeRollText = ""
        if (pl.isBot && pl.police > 0 && calcJailExit(g)) {
            pl.money -= 500
            pl.police = 0
            beforeRollText = g.buildMessage(
              "заплатил $500 чтобы выйти из тюрьмы",
              "you paid $500 to exit from jail"
            )
            g.logx("_paid_500_and_go_from_jail")
        }
        var rolls = s"($r1,$r2)"
        var plInfo = s"${pl.name}(money: ${pl.money})"
        var endText = beforeRollText + GameTexts
            .get(g.UILanguage, "_player_rolled")
            .format(plInfo, rolls)
        g.addRoundMessage(endText)

        if (pl.police > 0) {
            if (r1 == r2) {
                g.addRoundMessage(
                  "вы выходите из тюрьмы по дублю",
                  "you exit from jail because of double roll"
                )
                pl.police = 0
            } else {
                pl.police += 1
                if (pl.police == 4) {
                    g.addRoundMessage(
                      "вы должны заплатить $500 чтобы выйти из тюрьмы",
                      "you must pay $500 to go from jail"
                    )
                    g.toPayAmount(500, false);
                    return "_pay500_go";
                } else {
                    g.addRoundMessage(
                      "вы пропускаете ход в тюрьме",
                      "you passed turn"
                    );
                    return "_police_:not_roll_doudle";
                }
            }
        }

        if (checkOnTripple(pl.rolls)) {
            pl.pos = 10;
            pl.police = 1;
            // pl.rolls.addOne(0);
            return "_tripple_roll";
        }
        return "go"
    }

    def calcJailExit(g: Game): Boolean = true

    def checkOnTripple(steps: ListBuffer[Int]): Boolean = {
        if (steps.length > 2)
            steps.takeRight(3).forall(Game.DOUBLE_ROLLS.contains(_))
        else
            false
    }

    def processPosition(g: Game): Unit = {
        var p = g.currPlayer;
        var cell = g.currCell;
        if (cell.isLand())
            processLand(g, p, cell)
        else if (cell.ctype == 6) // tax cells
            g.toPayAmount(cell.rent());
        else if (cell.ctype == 4) // Chest cells
            processChestCard(g, p);
        else if (p.pos == 30) {
            p.pos = 10
            p.police = 1
            g.finishStep("_go_jail_after_30")
        } else
            g.finishStep("_no_functional_cell")
    }

    def processLand(g: Game, pl: Player, cell: Cell): Unit = {
        g.methodsTrace.addOne(s"[ProcessLand] cell:${cell.id}")

        if (cell.isNoOwner()) {
            g.addRoundMessage(
              s"Вы можете купить эту землю ${g.currCell.title} за ${g.currCell.cost}",
              s"You can buy this cell ${g.currCell.title}"
            )
            g.toCanBuy()
        } else if (cell.owner != pl.id) {
            if (cell.isMortgage)
                g.finishStep("_cell_mortgaged");
            else {
                g.payToUser = cell.owner;
                g.addRoundMessage(
                  s"заплатите ренту ${cell.rent()}",
                  s"pay rent ${cell.rent()}"
                );
                g.toPayAmount(cell.rent());

            }
        } else if (cell.owner == pl.id) {
            g.addRoundMessage(
              "вы попали на свою землю",
              "you visited your own cell"
            );
            g.finishStep(s"_mycell ${cell.title}");
        }
    }

    def processChestCard(g: Game, p: Player): Unit = {
        g.map.takeRandomCard();
        g.methodsTrace.addOne(
          s"[ProcessChestCard] card ${g.lastRandomCard.text}"
        );

        var card = g.lastRandomCard
        g.addRoundMessageByLabel("_random_took_card", card.text);

        card.randomGroup match {
            // получить мани
            case 1 => {
                p.money += card.money
                g.finishAfterChestCard()
            }
            // заплатить
            case 12 => {
                g.toPayAmount(card.money)
                g.finishAfterChestCard()
            }
            case 2 | 3 => g.moveToCell()

            case 4 => {
                g.payAmount = card.money * g.Players.length
                g.Players.foreach(pl => pl.money += card.money)
                g.toPay()
            }
            case 5 => {
                p.policeKey += 1
                g.finishAfterChestCard()
            }
            case 15 => {
                var hh = g.map.GetHotelsAndHouses(p.id)
                g.payAmount = 400 * hh.hotels + 100 * hh.houses
                g.toPay()
            }
            case _ => g.finishStep("finish_unknown_random")
        }
    }

    def moveAfterRandom(g: Game): Unit = {
        var c = g.lastRandomCard
        g.methodsTrace.addOne(
          s"[MoveAfterRandom] group:${c.randomGroup} ${c.text}"
        );

        var pl = g.currPlayer
        if (c.randomGroup == 1) {
            g.finishStep("_after_chest_card")
        } else if (c.randomGroup == 2 && c.pos == 10) {
            pl.pos = 10;
            pl.police = 1;
            g.addRoundMessage(
              "мусора вас забрали в тюрьму!",
              "you went to police jain after chest card"
            )
        } else if (c.randomGroup == 2) {
            if (pl.pos > c.pos) {
                pl.money += 2000
                g.addRoundMessage(
                  "вы прошли старт и получили $2000",
                  "you passed start and got $2000"
                )
            }
            pl.pos = c.pos
            processPosition(g)
        } else if (c.randomGroup == 3) {
            if (pl.pos > 3) pl.pos -= 3
            processPosition(g);
        } else {
            // g.AddRoundMessage($"неизвестная карточка {c.Text}", $"undefined card {c.Text}");
            g.finishStep("_no_move_chest_card_")
        }
    }

}
