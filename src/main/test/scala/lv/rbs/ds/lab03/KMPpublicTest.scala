package lv.rbs.ds.lab03

import org.scalatest._

class KMPpublicTest  extends FunSuite {
  test("this is a simple test") {
    var matcher = new KMPmatcher("ABCDABD")
    println(matcher.getPrefixFun())


  }
}
