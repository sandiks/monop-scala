package monoplib

object GameTexts {
    def get(lang:String, key:String):String = if lang == "ru" then getRuText(key) else getEnText(key)
    def getRuText(key:String):String = key
    def getEnText(key:String):String = key
}
