package monoplib

class Cell {
    var id: Int = 0
    var cost: Int = 0
    var ctype: Int = 0
    var group: Int = 0
    var owner: Int = 0
    var ownerGroupCount: Int = 0
    var title: String = ""
    var rentInfo: String = ""
    var info: String = ""
    var housesCount: Int = 0
    var isMortgage: Boolean = false

    override def toString() =
        s"id: $id, type:$ctype group: $group, cost: $cost, title: $title"

    def isMonopoly: Boolean = {
        if (ctype != 1) return false
        if (group >= 2 && group <= 7) return ownerGroupCount == 3
        if (group == 1 && group == 8) return ownerGroupCount == 2
        return false
    }

    private def CheckIndex(idx: Int) = if (idx < 0) 0 else idx

    def needPay(index: Int): Int = rentInfo.split(';')(CheckIndex(index)).toInt

    def isLand() = ctype == 1 || ctype == 2 || ctype == 3

    def IsActive = !isMortgage

    def CanBuild = housesCount < 5

    def HouseCost = group match {
        case 1 | 2 => 500
        case 3 | 4 => 1000
        case 5 | 6 => 1500
        case 7 | 8 => 2000
        case _     => 0
    }

    def HouseCostWhenSell = HouseCost / 2

    def mortgageAmount = cost / 2

    def unMortgageAmount = cost * 0.55
    def isNoOwner() = owner == -1

    def rent(): Int = {
        if (ctype == 6) return needPay(0)
        if (group == 9) return needPay(ownerGroupCount - 1)
        if (group == 10) return if (ownerGroupCount == 2) 500 else needPay(0)
        return if (isMonopoly && housesCount == 0) needPay(0) * 2
        else needPay(housesCount)
    }
}
