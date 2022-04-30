package porcEpic

import specification.{Etcd, EtcdTest}
import specification.Etcd.specification
import porcEpic.parser.EtcdParser

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class EtcdBench {

  val parsedEntries = scala.collection.mutable.Map.empty[String, List[Entry[Etcd.Input, Etcd.Output]]]

  EtcdTest.names.foreach{name =>
    val filename = leftPad(name)(3, '0')
    parsedEntries(filename) = EtcdParser.parseFile(filename)
  }

  def test(n: String): Unit = {
    val entries = parsedEntries(n)
    specification.checkEntries(entries)
  }

  @Benchmark def etcd_000: Unit = test("000")
  @Benchmark def etcd_001: Unit = test("001")
  @Benchmark def etcd_002: Unit = test("002")
  @Benchmark def etcd_003: Unit = test("003")
  @Benchmark def etcd_004: Unit = test("004")
  @Benchmark def etcd_005: Unit = test("005")
  @Benchmark def etcd_006: Unit = test("006")
  @Benchmark def etcd_007: Unit = test("007")
  @Benchmark def etcd_008: Unit = test("008")
  @Benchmark def etcd_009: Unit = test("009")
  @Benchmark def etcd_010: Unit = test("010")
  @Benchmark def etcd_011: Unit = test("011")
  @Benchmark def etcd_012: Unit = test("012")
  @Benchmark def etcd_013: Unit = test("013")
  @Benchmark def etcd_014: Unit = test("014")
  @Benchmark def etcd_015: Unit = test("015")
  @Benchmark def etcd_016: Unit = test("016")
  @Benchmark def etcd_017: Unit = test("017")
  @Benchmark def etcd_018: Unit = test("018")
  @Benchmark def etcd_019: Unit = test("019")
  @Benchmark def etcd_020: Unit = test("020")
  @Benchmark def etcd_021: Unit = test("021")
  @Benchmark def etcd_022: Unit = test("022")
  @Benchmark def etcd_023: Unit = test("023")
  @Benchmark def etcd_024: Unit = test("024")
  @Benchmark def etcd_025: Unit = test("025")
  @Benchmark def etcd_026: Unit = test("026")
  @Benchmark def etcd_027: Unit = test("027")
  @Benchmark def etcd_028: Unit = test("028")
  @Benchmark def etcd_029: Unit = test("029")
  @Benchmark def etcd_030: Unit = test("030")
  @Benchmark def etcd_031: Unit = test("031")
  @Benchmark def etcd_032: Unit = test("032")
  @Benchmark def etcd_033: Unit = test("033")
  @Benchmark def etcd_034: Unit = test("034")
  @Benchmark def etcd_035: Unit = test("035")
  @Benchmark def etcd_036: Unit = test("036")
  @Benchmark def etcd_037: Unit = test("037")
  @Benchmark def etcd_038: Unit = test("038")
  @Benchmark def etcd_039: Unit = test("039")
  @Benchmark def etcd_040: Unit = test("040")
  @Benchmark def etcd_041: Unit = test("041")
  @Benchmark def etcd_042: Unit = test("042")
  @Benchmark def etcd_043: Unit = test("043")
  @Benchmark def etcd_044: Unit = test("044")
  @Benchmark def etcd_045: Unit = test("045")
  @Benchmark def etcd_046: Unit = test("046")
  @Benchmark def etcd_047: Unit = test("047")
  @Benchmark def etcd_048: Unit = test("048")
  @Benchmark def etcd_049: Unit = test("049")
  @Benchmark def etcd_050: Unit = test("050")
  @Benchmark def etcd_051: Unit = test("051")
  @Benchmark def etcd_052: Unit = test("052")
  @Benchmark def etcd_053: Unit = test("053")
  @Benchmark def etcd_054: Unit = test("054")
  @Benchmark def etcd_055: Unit = test("055")
  @Benchmark def etcd_056: Unit = test("056")
  @Benchmark def etcd_057: Unit = test("057")
  @Benchmark def etcd_058: Unit = test("058")
  @Benchmark def etcd_059: Unit = test("059")
  @Benchmark def etcd_060: Unit = test("060")
  @Benchmark def etcd_061: Unit = test("061")
  @Benchmark def etcd_062: Unit = test("062")
  @Benchmark def etcd_063: Unit = test("063")
  @Benchmark def etcd_064: Unit = test("064")
  @Benchmark def etcd_065: Unit = test("065")
  @Benchmark def etcd_066: Unit = test("066")
  @Benchmark def etcd_067: Unit = test("067")
  @Benchmark def etcd_068: Unit = test("068")
  @Benchmark def etcd_069: Unit = test("069")
  @Benchmark def etcd_070: Unit = test("070")
  @Benchmark def etcd_071: Unit = test("071")
  @Benchmark def etcd_072: Unit = test("072")
  @Benchmark def etcd_073: Unit = test("073")
  @Benchmark def etcd_074: Unit = test("074")
  @Benchmark def etcd_075: Unit = test("075")
  @Benchmark def etcd_076: Unit = test("076")
  @Benchmark def etcd_077: Unit = test("077")
  @Benchmark def etcd_078: Unit = test("078")
  @Benchmark def etcd_079: Unit = test("079")
  @Benchmark def etcd_080: Unit = test("080")
  @Benchmark def etcd_081: Unit = test("081")
  @Benchmark def etcd_082: Unit = test("082")
  @Benchmark def etcd_083: Unit = test("083")
  @Benchmark def etcd_084: Unit = test("084")
  @Benchmark def etcd_085: Unit = test("085")
  @Benchmark def etcd_086: Unit = test("086")
  @Benchmark def etcd_087: Unit = test("087")
  @Benchmark def etcd_088: Unit = test("088")
  @Benchmark def etcd_089: Unit = test("089")
  @Benchmark def etcd_090: Unit = test("090")
  @Benchmark def etcd_091: Unit = test("091")
  @Benchmark def etcd_092: Unit = test("092")
  @Benchmark def etcd_093: Unit = test("093")
  @Benchmark def etcd_094: Unit = test("094")
  @Benchmark def etcd_095: Unit = test("095")
  @Benchmark def etcd_096: Unit = test("096")
  @Benchmark def etcd_097: Unit = test("097")
  @Benchmark def etcd_098: Unit = test("098")
  @Benchmark def etcd_099: Unit = test("099")
  @Benchmark def etcd_100: Unit = test("100")
  @Benchmark def etcd_101: Unit = test("101")
  @Benchmark def etcd_102: Unit = test("102")
}