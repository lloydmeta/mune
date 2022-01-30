class EnumMacrosTest extends munit.FunSuite {
  test("values should find members in order") {
    assertEquals(
      SomeEnum.values,
      IndexedSeq(SomeEnum.SomeEnumA, SomeEnum.SomeEnumB, SomeEnum.Sub.SomeEnumC)
    )
  }

  test("materialise enums should work") {
    val entry: SomeEnum = SomeEnum.SomeEnumA
    assertEquals(SomeEnum: TestEnum[SomeEnum], getEnumForEntry(entry))
  }

  test("materialise enums should *not* work") {
    compileErrors(
      """
      val entry: EnumTypeWithNoCompanion = EnumMemberWithNoCompanion
      assertEquals(SomeEnum, getEnumForEntry(entry))
      """
    )
  }

}

trait TestEnumEntry

trait TestEnum[T <: TestEnumEntry] {

  def values: IndexedSeq[T]

  inline protected def findValues: IndexedSeq[T] = EnumMacros.findValuesImpl[T]

}

object TestEnum {
  implicit inline def materializeEnum[A <: TestEnumEntry]: TestEnum[A] =
    EnumMacros.materializeEnumImpl[TestEnum[A]]
}

def getEnumForEntry[A <: TestEnumEntry: TestEnum](a: A): TestEnum[A] = implicitly[TestEnum[A]]

sealed trait SomeEnum extends TestEnumEntry

object SomeEnum extends TestEnum[SomeEnum] {
  case object SomeEnumA extends SomeEnum
  case object SomeEnumB extends SomeEnum
  object Sub {
    case object SomeEnumC extends SomeEnum
  }

  val values = findValues
}

sealed trait EnumTypeWithNoCompanion extends TestEnumEntry

case object EnumMemberWithNoCompanion extends EnumTypeWithNoCompanion
