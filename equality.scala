// multiversal equality



def test_equality1=
  case class Apple(size: Int)
  case class Orange(size: Int)
  val appleTwo = Apple(2)
  val appleTwoToo = Apple(2)
  assert(appleTwo == appleTwoToo) // true
  val orangeTwo = Orange(2)
  // Scala 3 compiler will also still by default allow comparisons of types for
  // which no given reflexive CanEqual instances exist, the following
  // undesirable equality comparison would still compile:
  assert(appleTwo != orangeTwo) // false
  import scala.language.strictEquality
  // throw error
  // assert(appleTwo != orangeTwo)
def test_equality2=
  import scala.language.strictEquality
  case class Apple(size: Int)
  // provide an explicit given instance in the Apple companion object
  object Apple:
    given canEq:CanEqual[Apple,Apple]=CanEqual.derived

  val appleTwo = Apple(2)
  val appleTwoToo = Apple(2)
  assert(appleTwo == appleTwoToo) // true

def test_equality3=
  import scala.language.strictEquality
  // get an instance of the CanEqual typeclass derived for your Apple instance
  case class Apple(size: Int) derives CanEqual

  val appleTwo = Apple(2)
  val appleTwoToo = Apple(2)
  assert(appleTwo == appleTwoToo) // true

def type_class_derivation=
  enum Tree[T] derives Eq, Ordering, Show:
    case Branch(left: Tree[T], right: Tree[T])
    case Leaf(elem: T)
  // derives clause generates the following given instances for the Eq,
  // Ordering and Show type classes in the companion object of Tree,

  // given [T: Eq]       : Eq[Tree[T]]    = Eq.derived
  // given [T: Ordering] : Ordering[Tree] = Ordering.derived
  // given [T: Show]     : Show[Tree]     = Show.derived

