package porcEpic
package integration

import org.scalatest.funsuite.AnyFunSuite

import specification.Etcd._
import parser.EtcdParser

class EtcdTest extends AnyFunSuite {

  // 102
  (0 to 102)
    .filter(_ == 2)
    .filterNot(_ == 95)
    .map(_.toString)
    // .filter(x => results(x))
    .foreach(name =>

    test(s"etcd $name") {
      // println(s"\n-- etcd $name --")
      val entries = EtcdParser.parseFile(name)
      val (obtained, info) = specification.checkEntries(entries)//, verbosity = Verbosity.Debug)
      println(info.partialLinearizations.map(_.map(_.toList).toList).toList)
      val expected = 
        if (!results(name)) CheckResult.Illegal
        else CheckResult.Ok
      assert(obtained == expected)
    }
  )

  lazy val results = Map(
      "0" -> false,
      "1" -> false,
      "2" -> true,
      "3" -> false,
      "4" -> false,
      "5" -> true,
      "6" -> false,
      "7" -> true,
      "8" -> false,
      "9" -> false,
     "10" -> false,
     "11" -> false,
     "12" -> false,
     "13" -> false,
     "14" -> false,
     "15" -> false,
     "16" -> false,
     "17" -> false,
     "18" -> true,
     "19" -> false,
     "20" -> false,
     "21" -> false,
     "22" -> false,
     "23" -> false,
     "24" -> false,
     "25" -> true,
     "26" -> false,
     "27" -> false,
     "28" -> false,
     "29" -> false,
     "30" -> false,
     "31" -> true,
     "32" -> false,
     "33" -> false,
     "34" -> false,
     "35" -> false,
     "36" -> false,
     "37" -> false,
     "38" -> true,
     "39" -> false,
     "40" -> false,
     "41" -> false,
     "42" -> false,
     "43" -> false,
     "44" -> false,
     "45" -> true,
     "46" -> false,
     "47" -> false,
     "48" -> true,
     "49" -> true,
     "50" -> false,
     "51" -> true,
     "52" -> false,
     "53" -> true,
     "54" -> false,
     "55" -> false,
     "56" -> true,
     "57" -> false,
     "58" -> false,
     "59" -> false,
     "60" -> false,
     "61" -> false,
     "62" -> false,
     "63" -> false,
     "64" -> false,
     "65" -> false,
     "66" -> false,
     "67" -> true,
     "68" -> false,
     "69" -> false,
     "70" -> false,
     "71" -> false,
     "72" -> false,
     "73" -> false,
     "74" -> false,
     "75" -> true,
     "76" -> true,
     "77" -> false,
     "78" -> false,
     "79" -> false,
     "80" -> true,
     "81" -> false,
     "82" -> false,
     "83" -> false,
     "84" -> false,
     "85" -> false,
     "86" -> false,
     "87" -> true,
     "88" -> false,
     "89" -> false,
     "90" -> false,
     "91" -> false,
     "92" -> true,
     "93" -> false,
     "94" -> false,
     "96" -> false,
     "97" -> false,
     "98" -> true,
     "99" -> false,
    "100" -> true,
    "101" -> true,
    "102" -> true,
  )
}
