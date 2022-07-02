package myApp.semigroup

import scala.annotation.targetName
// A semigroup is defined loosely as a set + a combination function which takes
// two elements of that set and produces a third, still from the set.
trait Semigroup[T]:
  def combine(a: T, b: T): T
object my
object My_semigroup:
  val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def combine(a: Int, b: Int) = a + b
  }

  val stringSemigroup: Semigroup[String] = new Semigroup[String] {
    override def combine(a: String, b: String) = a + b
  }
object SemigroupInstances {
  given intSemigroup: Semigroup[Int] with
    override def combine(a: Int, b: Int) = a + b

  given stringSemigroup: Semigroup[String] with
    override def combine(a: String, b: String) = a + b
}
object Semigroup {
  def apply[T](using instance: Semigroup[T]): Semigroup[T] = instance
}
import SemigroupInstances.{stringSemigroup, intSemigroup}
val naturalIntSemigroup = Semigroup[Int]
val naturalStringSemigroup = Semigroup[String]

val meaningOfLife = naturalIntSemigroup.combine(2, 40)
val language = naturalStringSemigroup.combine("Sca", "la")
def semigroup_ex =
  println(meaningOfLife)
  println(language)

trait Semigroup[T]:
  extension (t: T)
    infix def combine(other: T): T
    @targetName("plus") def <+>(other: T): T = t.combine(other)
trait Monoid[T] extends Semigroup[T]:
  def unit: T
given StringMonoid: Monoid[String] with
  def unit: String = ""
  extension (s: String) infix def combine(other: String): String = s + other
given IntMonoid: Monoid[Int] with
  def unit: Int = 0
  extension (i: Int) infix def combine(other: Int): Int = i + other

// type [T : Numeric] is a context bound, a shorthand way
//
object My_numreic:
  given NumericMonoid[T](using num: Numeric[T]): Monoid[T] with
    def unit: T = num.zero
    extension (t: T) infix def combine(other: T): T = num.plus(t, other)
given NumericMonoid[T: Numeric]: Monoid[T] with
  // used summon to get the anonymous using parameter
  // you can write summon[Foo] whenever you want to find a given instance of
  // type Foo in the current scope
  def unit: T = summon[Numeric[T]].zero
  extension (t: T)
    infix def combine(other: T): T = summon[Numeric[T]].plus(t, other)


// With the type parameter T, NumericMonoid must be a class for which instances
// will be created by the compiler when T is specified. In contrast, an object
// is created for IntMonoid and StringMonoid.

def test_monoid =
  // righthand side could be written NumericMonoid[BigDecimal].unit, but
  // Big Decimal can be inferred. This doesn’t work for the next two lines
  // because the Monoid.unit is the object on which the methods are called.
  val a = BigDecimal(3.14) <+> NumericMonoid.unit
  val b = NumericMonoid[BigDecimal].unit <+> BigDecimal(3.14)
  val c = NumericMonoid[BigDecimal].unit combine BigDecimal(3.14)
