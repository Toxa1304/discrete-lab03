//package lv.rbs.ds.lab03
//
//import net.liftweb.json.JsonDSL._
//import net.liftweb.json._
//
//class NAIVEmatcher(var pattern:String) {
//  val m = pattern.length
//
//  /**
//   * The Naive search algorithm as in a textbook
//   * @param text The text for searching
//   * @return List with all the offsets, where pattern is in the text.
//   *         Or an empty iterator, if the pattern was not found.
//   */
//  def findAllIn(text: CharSequence):  Iterator[Int] = {
//    val n = text.length
//    if (n < m) {
//      Iterator.empty
//    } else {
//      val seq = for {
//        i <- 0 to (n - m)
//        if (0 until m).foldLeft(true)(
//          (isMatch, j) => {
//            isMatch && text.charAt(i + j) == pattern.charAt(j)
//          }
//        )
//      } yield i
//      seq.iterator
//    }
//  }
//
//  /**
//   * This algorithm is very similar to the findAllIn,
//   * but it returns a list of 4-tuples: (offset, start, end, isMatch).
//   * These 4-tuples are needed to fill in JSON data.
//   *
//   * Offset 'i' belongs to the iterator returned by 'findAllIn' iff
//   * 'findAllSearchSteps' contains a tuple (i,*,*,1) - namely, there is a match at position i.
//   */
//  def findAllSearchSteps(text: CharSequence): List[(Int,Int,Int,Int)] = {
//    val n = text.length
//    val steps = for {
//      i <- 0 to (n - m)
//      // Find the smallest index of a mismatch
//      mismatch = (0 until m).find(j => text.charAt(i + j) != pattern.charAt(j))
//      // Sort 2 cases:
//      // EITHER there was a mismatch - add it to the result list up to the 'idx'
//      // OR there was no mismatch - add it to the result as a wholly matched pattern
//      quadruplet = mismatch match {
//        case Some(idx) => (i,0,idx,0)
//        case None => (i,0,m-1,1)
//      }
//    } yield quadruplet
//    steps.toList
//  }
//
//  def toJson(text: CharSequence): String = {
//    val transcript = new SearchTranscript("Naive", pattern, text, findAllSearchSteps(text))
//    transcript.getJson()
//  }
//}
