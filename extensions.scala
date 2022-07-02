package myApp.extension_examples

import scala.annotation.targetName

// The first form puts all the needed type parameters, [A,B] in this case,
// after the extension keyword. The second form, which looks closer to what we
// already know for parameterized types and methods
extension [A, B](a: A)
  // @targetName can’t be used in Scala code (e.g., a.arrow2(b)), but it can be
  // used in Java code to call the method. This is the new recommendation for
  // all operator methods
  @targetName("arrowScala3") def ~>(b: B): (A, B) = (a, b)

extension [A](a: A)
  @targetName("arrowScala3_2") def ~~>[B](b: B): (A, B) = (a, b)

// extension methods can call directly
extension (s: String)
  def single_whitespace: String =
    s.trim().split("\\s+").mkString(" ")

extension [T](xs: List[T])
  def tailOption: Option[List[T]] =
    if xs.nonEmpty then Some(xs.tail) else None

extension (n: Int)
  def absOption: Option[Int] =
    if n != Int.MinValue then Some(n.abs) else None
  def negateOption: Option[Int] =
    if n != Int.MinValue then Some(-n) else None

// extension on case class
object MyOperator:
  @targetName("TIEFighter") case class <+>[A, B](a: A, b: B)

  extension [A](a: A) def <+>[B](b: B): A <+> B = new <+>(a, b)

def ex4 =
  import MyOperator.*
  val a = "a" <+> 1
  val b = <+>("a", 1)
// extension on object
object OneProject:
  def one = 1
extension (onep: OneProject.type)
  def add(b: Int) =
    b + onep.one
def ex5 =
  val a = OneProject.add(5)

object SimpleSQL:
  case class SQL_query(columns: Seq[String] = Nil, table: String = "")
  extension (sc: StringContext)
    def sql2(values: String*): SQL_query =
      def tup2seq(t: (String, String)) = Seq(t._1, t._2)
      def keep_str(s: String) = s.length > 0 && s != "SELECT" && s != "FROM"
      val strs = sc.parts.toVector.zipAll(values, "", "").flatMap(tup2seq)
      val tokens =
        strs.flatMap(_.split("[\\s,:]+")).map(_.trim).filter(keep_str)
      val columns = tokens.take(tokens.size - 1)
      val table = tokens.last
      SQL_query(columns, table)
  extension (sc: StringContext)
    def sql: SQL_query =
      // tuple to sequence
      def tup2seq(t: (String, String)) = Seq(t._1, t._2)
      def keep_str(s: String) = s.length > 0 && s != "SELECT" && s != "FROM"
      val tokens = sc.parts.toVector
        .flatMap(_.split("[\\s,:]+").toList)
        .map(_.trim)
        .filter(keep_str)
      val columns = tokens.take(tokens.size - 1)
      val table = tokens.last
      SQL_query(columns, table)
def test_sql =
  import SimpleSQL.*
  val query = sql"SELECT one, two FROM t"
  query == SQL_query(Vector("one", "two"), "t")
def test_flatmap=
  val s=List("a:b:c","d")
  println(s.flatMap(_.split(":").toList))
