package lv.rbs.ds.lab03

class BMmatcher(pattern: String) {

  /**
   * This method should return a list of length m (length of the pattern).
   * Good Suffix function
   */
  def getGoodSuffixFun(): Map[Int, Int] = {
    val strOps = pattern.reverse

    /* create a list of valid characters from the str */
    val strList = strOps.foldLeft(List[Char]()) {
      case (l, ch) if !l.contains(ch) => ch :: l
      case (l, ch) => l
    }

    val result = strOps.foldLeft(Map[Int, Int](), 0, "") {
      case ((mMap, pos, subStr), ch) if pos != 0 => (mMap ++ Map(pos -> calculate(pos, subStr, `pattern`,
        /* a valid characters list is reconstructed again without its actual character */
        `strList`.filter {
          case x if x == ch => false
          case _ => true
        })), pos + 1, ch + subStr)
      case ((mMap, pos, subStr), ch) => (Map(0 -> 1), pos + 1, ch + subStr)
    }
    result._1
  }

  def getBadCharFun(): Map[Char,Int] = {
    /* reverse: to use foldLeft,
      tail: the last(now head) charcter, which is not neede, is dropped */
    val result = pattern.reverse.tail.foldLeft(Map[Char,Int](),1)  {
      case ((mMap,pos),ch) if ! mMap.isDefinedAt(ch) => (mMap ++ Map(ch -> pos), pos+1)
      case ((mMap,pos),ch)                           => (mMap,pos +1)
    }
    return result._1

  }

  def findAllIn(text: CharSequence): Iterator[Int] = {
    List().iterator
  }

  def toJson(text: CharSequence): String = {
    ""
  }


  def calculate(pos : Int, subStr : String,
                str : String, strList : List[Char]) : Int = {

    /* strList will be empty when the recursive call to this function
        for backtracking subStr is invoked*/
    if (strList.isEmpty) {
      val jListMin = subSetJump(subStr, str)

      /* the current subStr is not a valid sub string,
        backtrack 1 and calculate to find again*/
      if (jListMin < 0)
        return calculate(pos - 1, subStr.tail,
          str, List[Char]())

      /* the current subStr is a valid sub String
          but the jump is 0 and no moving. backtrack 1 and calculate */
      if (str.length - 1 - pos - jListMin == 0)
        return calculate(pos - 1, subStr.tail,
          str, List[Char]())

      /* difference of current pos(counting from front)
        with the position first of the first valid subset */
      return str.length - 1 - pos - jListMin
    }

    /* get the positions of all valid sub strings
      constructed with valid characters */
    val jList = strList.foldLeft(List[Int]()) {
      (j, ch) => subSetJump(ch + `subStr`, `str`) :: j
    }
    /* jList.max is less than 0, the rest will be the same */
    if (jList.max < 0) {
      if (subStr.length == 1)	return str.length
      return calculate(pos - 1, subStr,
        str, List[Char]())
    }

    /* we will use the minimum positive value including 0*/
    val jListmin=(jList.filter{
      case x if x >= 0 => true
      case x           => false}).min

    if (str.length - 1 - pos - jListmin == 0)
      return calculate(pos - 1, subStr.tail,
        str, List[Char]())
    return str.length - 1 - pos - jListmin
  }
  def subSetJump(subStr : String, str : String) : Int = {
    val list = (for (i <- 0 until str.length)
      yield str.slice(i, i + subStr.length)).toList
    list.indexOf(subStr)
  }
}
