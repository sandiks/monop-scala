package monoplib

case class HotelsHouses(hotels: Int, houses: Int)

class Map(var g: Game) {
    val rand = new scala.util.Random

    def CellsByUser(pid: Int) = g.Cells.filter(_.owner == pid)
    def CellsByGroup(group: Int) = g.Cells.filter(_.group == group)
    def CellsByType(cellType: Int) = g.Cells.filter(_.ctype == cellType)
    def CellsByUserByGroup(pid: Int, group: Int) =
        g.Cells.filter(c => c.owner == pid && c.group == group)
    def CellsByUserByType(pid: Int, ctype: Int) =
        g.Cells.filter(c => c.owner == pid && c.ctype == ctype)

    def GetHotelsAndHouses(pid: Int): HotelsHouses = {
        val cc = CellsByUserByType(pid, 1)
        var houses = cc
            .filter(c => c.housesCount > 0 && c.housesCount < 5)
            .foldLeft(0)(_ + _.housesCount)
        var hotels =
            cc.filter(_.housesCount == 5).foldLeft(0)(_ + _.housesCount)
        return HotelsHouses(hotels, houses)
    }

    def MonopGroupsByUser(pid: Int) = CellsByUserByType(pid, 1)
        .filter(_.isMonopoly)
        .groupBy(_.group)
        .filter(x => x._2.forall(_.IsActive))

    def setOwner(p: Player, cell: Cell, cost: Int): Unit = {
        if (cell.owner == p.id) return
        cell.owner = p.id
        cell.isMortgage = false
        p.money -= cell.cost
        updateCellsGroupInfo()
    }

    def updateCellsGroupInfo() = {
        var groups = g.Cells
            .filter(x => x.isLand() && x.owner != -1)
            .groupBy(x => (x.group, x.owner));
        groups.foreach(group =>
            group._2.foreach(gcell => gcell.ownerGroupCount = group._2.size)
            g.PlayerCellGroups(group._1(0))(group._1(1)) = group._2.size
        )
    }

    def takeRandomCard() = {
        if (List(7, 22, 36) contains g.currCell.id) {
            val count = g.ChanceChest.size
            g.lastRandomCard = g.ChanceChest(rand.nextInt(count))
        } else {
            val count = g.CommunityChest.size
            g.lastRandomCard = g.CommunityChest(rand.nextInt(count))
        }
    }
}
