import org.junit.Test
import org.junit.Assert.*

class Test1:
  @Test def t1(): Unit =
    assertEquals("I was compiled by Scala 3. :)", msg)

  @Test def t2(): Unit =
    assertEquals(SomeEnum.values, IndexedSeq(SomeEnum.SomeEnumA, SomeEnum.SomeEnumB, SomeEnum.Sub.SomeEnumC))

  @Test def t3(): Unit =
    val entry: SomeEnum = SomeEnum.SomeEnumA
    assertEquals(SomeEnum, getEnumForEntry(entry))


trait TestEnumEntry

trait TestEnum[T <: TestEnumEntry] {

  def values: IndexedSeq[T]

  inline protected def findValues: IndexedSeq[T] = EnumMacros.findValuesImpl[T]


}

inline def getEnumForEntry[A <: TestEnumEntry](a: A): TestEnum[A] = EnumMacros.materializeEnumImpl[TestEnum[A]]

sealed trait SomeEnum extends TestEnumEntry

object SomeEnum extends TestEnum[SomeEnum] {
  case object SomeEnumA extends SomeEnum
  case object SomeEnumB extends SomeEnum
  object Sub {
    case object SomeEnumC extends SomeEnum
  }

  val values = findValues
}


