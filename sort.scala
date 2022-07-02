package myApp.sortFunction

trait Ord[T]:
  def compare(x: T, y: T): Int
  def lteq(x: T, y: T) = compare(x, y) <=0
object IntOrd:
  given intOrd1: Ord[Int] with
    def compare(x: Int, y: Int): Int =
        compare(x,y)
object Ord:
  // make intOrd1 private to solve conflict
  private given intOrd1: Ord[Int] =
    new Ord[Int]:
      def compare(x: Int, y: Int): Int =
        if x == y then 0 else if x > y then 1 else -1
  given intOrd: Ord[Int] with
    def compare(x: Int, y: Int): Int =
      if x == y then 0 else if x > y then 1 else -1
// For sorting Ints, that parameter type is Ord[Int]. Although the com- piler
// will first look in lexical scope for a given Ord[Int], if it does not find
// any, it will look as a second step in the companion objects of the involved
// types Ord, Int, and the companion objects of their supertypes.

// add to object Ord
given stringOrd:Ord[String] with
  def compare(x: String, y: String): Int = x.compareTo(y)

// If a given declaration does not take value parameters, then that given is
// initialized the first time it’s accessed, similar to a lazy val. That
// initialization is performed in a thread-safe manner. If a given does take
// parameters, then a new given is created on every access, much like how a def
// behaves. Indeed, the Scala compiler transforms givens to lazy vals or defs,
// additionally marking them as being available for using parameters.


def insert[T](x: T, ls: List[T])(using order: Ord[T]): List[T] =
  if ls.isEmpty || order.lteq(x, ls.head) then x :: ls
  else ls.head :: insert(x, ls.tail)
def isort[T](ls: List[T])(using order: Ord[T]): List[T] =
  if ls.isEmpty then Nil
  else insert(ls.head, ls.tail)
