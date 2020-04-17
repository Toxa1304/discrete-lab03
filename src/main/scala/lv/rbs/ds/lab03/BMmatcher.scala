package lv.rbs.ds.lab03


import play.api.libs.json.{JsValue, Json}

class BMmatcher(pattern:String) {
  var patternOcurred = pattern.toCharArray
  var comparasions:Int = 0
  var steps:List[Map[String, String]] = List()
  var ASCII_SIZE: Int = 256
  def max(a: Int, b: Int): Int = Math.max(a, b)

  def findAllIn(text: String): List[Int] = {
    var result:List[Int] = List()
    var txt = text.toCharArray
    val m: Int = patternOcurred.length
    val n: Int = txt.length
    // initiate array of bad characters
    val badchar: Array[Int] = Array.ofDim[Int](ASCII_SIZE)
    for (i <- 0 until ASCII_SIZE) {
      badchar(i) = -1
    }
    for (i <- 0 until m) {
      badchar(patternOcurred(i).toInt) = i
    }
    var s: Int = 0
    while (s <= (n - m)) {
      var j: Int = m - 1

      while (j >= 0 && patternOcurred(j) == txt(s + j)) j -= 1
      if (j < 0) {
        result = result :+ s
        if (s + m < n) s += m - badchar(txt(s + m))
        else s += 1
      }
      else {
        s += max(1, j - badchar(txt(s + j)))
      }
    }
    result
  }
  def getGoodSuffixFun(): List[(Int,Int)] = {
    val m: Int = patternOcurred.length
    val bpos: Array[Int] = Array.ofDim[Int](m + 1)
    val shift: Array[Int] = Array.ofDim[Int](m + 1)
    for (i <- 0 until m + 1) shift(i) = 0
    var i: Int = m
    var j: Int = m + 1
    bpos(i) = j
    while (i > 0) {
      while (j <= m && patternOcurred(i - 1) != patternOcurred(j - 1)) {
        if (shift(j) == 0) shift(j) = j - i
        j = bpos(j)
      }
      i -= 1
      j -= 1
      bpos(i) = j
    }
    j = bpos(0)
    i = 0

    while (i <= m) {
      if (shift(i) == 0) shift(i) = j

      if (i == j) j = bpos(j)
      i += 1
    }

    shift.toList
    var answer:List[(Int, Int)] = List()
    var counter = 0

    for(i <- shift){
      answer = answer :+ (counter, i)
      counter += 1
    }
    answer
  }

  def getBadCharFun(): List[(Char, Int)] = {
    val badchar: Array[Int] = Array.ofDim[Int](ASCII_SIZE)
    for (i <- patternOcurred.indices) {
      badchar(patternOcurred(i).toInt) = i
    }
    val a = badchar.filter(_ > 0)

    var list: List[(Char, Int)] = List()
    for (i <- badchar.indices) {
      if (badchar(i) > 0) {
        list = list :+ (i.toChar, badchar(i))
      }
    }
    list
  }

  def toJson(text: String): String = {
    var sufix = getGoodSuffixFun()
    findAllIn(text)
    val jsonMap: Map[String, JsValue] = Map(
      "algorithm" -> Json.toJson("BM"),
      "pattern" -> Json.toJson(pattern),
      "text" -> Json.toJson(text.toString),
      "prefixFun" -> Json.toJson(sufix),
      "steps" -> Json.toJson(steps),
      "comparisons" -> Json.toJson(comparasions)
    )
    val reply = Json.stringify(Json.toJson(jsonMap))
    reply
  }
}