def t1 =
  val res = (1 to 999).filter(x => x % 3 == 0 || x % 5 == 0).sum

def scan_left =
  assert(List(1, 2, 3, 4, 5).scanLeft(0)(_ + _) == List(0, 1, 3, 6, 10, 15))
  assert(List(1, 2, 3, 4, 5).scanRight(0)(_ + _) == List(15, 14, 12, 9, 5, 0))

def t2 =
  // val a1: List[Int] = List(0, 1, 1)
  // val a2: List[Int] = List(0, 1, 1, 2)
  // val a3: List[Int] = List(0, 1, 1, 2, 3)
  val a1 = 0 :: 1 :: List(0, 1).zip(List(0, 1).tail).map(n => n._1 + n._2)
  val a2 = 0 :: 1 :: a1.zip(a1.tail).map(n => n._1 + n._2)
  val a3 = 0 :: 1 :: a2.zip(a2.tail).map(n => n._1 + n._2)

  // generate fibonacci lazy list
  val fib: LazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #:: fib.zip(fib.tail).map(n => n._1 + n._2)

  val a1 = 0 :: List(0).scanLeft(1)(_ + _) // List(1, 1)
  val a2 = 0 :: a1.scanLeft(1)(_ + _) // List(1, 1, 2, 3)
  val a3 = 0 :: List(0, 1, 1, 2, 3).scanLeft(1)(_ + _) // List(1, 1, 2, 3, 5, 8)
  // solution 1
  val fib: LazyList[Int] = 0 #:: fib.scanLeft(1)((x, y) => x + y)
  //
  def fib(a: Int = 0, b: Int = 1): LazyList[Int] = a #:: fib(b, a + b)
  //
  def fib(a: Int = 0, b: Int = 1): LazyList[Int] =
    LazyList.cons(a, fib(b, a + b))

  val fib: LazyList[Int] = 0 #:: fib.scanLeft(1)(_ + _)

  val res = fib.view.takeWhile(_ <= 4_000_000).filter(_ % 2 == 0).sum

def big_number_sqrt =
  import java.math.*
  val a = BigDecimal(2).sqrt(java.math.MathContext(10))
  // val b=BigInt(2).sqrt(java.math.MathContext(20))

def t3 =
  import java.math.*
  // get smallest prime factor
  def f(n: BigInt): Option[BigInt] =
    (BigInt(2) to BigInt(BigInteger(n.toString).sqrt()))
      .find(n % _ == 0)
  def factors(n: BigInt): List[BigInt] =
    (BigInt(2) to BigInt(BigInteger(n.toString).sqrt()))
      .find(n % _ == 0)
      .fold(List(n))(i => i :: factors(n / i))
  val res = factors(BigInt("600851475143")).last

def t4 =
  val r = (100 to 999).view
    .flatMap(x => (100 to 999).map(y => y * x))
    .filter(n => n.toString == n.toString.reverse)
    .max

def t5 =
  val r = Iterator
    .from(20)
    .find(n => (2 to 20).forall(i => n % i == 0))
    .get
  val r = (20 to Int.MaxValue)
    .find(n => (2 to 20).forall(i => n % i == 0))
    .get

def t6 =
  def square(n: Int) = n * n
  val r = square((1 to 100).sum) - (1 to 100).map(square).sum

def t7 =

  val primes: LazyList[Int] = 2 #:: LazyList
    .from(3)
    .filter(i => primes.takeWhile(p => p * p <= i).forall(p => i % p != 0))
  val primes: LazyList[Int] = 2 #:: Iterator
    .from(3)
    .filter(i => primes.takeWhile(p => p * p <= i).forall(p => i % p != 0))

def t8 =
  val r = io.Source
    .fromFile("/Users/felix/euler-projects-test/euler8.txt")
    .mkString
    .filter(x => x.isDigit)
    .map(x => x.asDigit)
    .sliding(13)
    // to avoid integer overflow
    .map(x => x.map(i => BigInt(i)))
    .map(x => x.product)
    .max
def overflow_error =
  val a = Int.MaxValue + 1

def t9 =
  def square(x: Int) = x * x
  val r = for (
    a <- 1 until 1000;
    b <- 1 until a;
    c = 1000 - a - b;
    if square(a) + square(b) == square(c)
  ) yield a * b * c
  val res = r.head

def t10 =
  val primes: LazyList[Int] = 2 #:: LazyList
    .from(3)
    .filter(x => primes.takeWhile(i => i * i <= x).forall(i => x % i != 0))
  val r = primes.takeWhile(_ < 2_000_000).map(_.toLong).sum

def t12 =
  // get triangle sequence
  val a1 = 0 :: List(0).zipWithIndex.map(n => n._1 + n._2 + 1)
  val a2 = 0 :: a1.zipWithIndex.map(n => n._1 + n._2 + 1)
  val a3 = 0 :: a2.zipWithIndex.map(n => n._1 + n._2 + 1)

  val ls: LazyList[Int] = 0 #:: ls.zipWithIndex.map(n => n._1 + n._2 + 1)

  val ls = Iterator.from(1).map(n => (1 to n).sum)
  val ls = Iterator.from(1).map(n => (n + 1) * n / 2)

  val ls = (1 to Int.MaxValue).view.scanLeft(0)(_ + _).tail
  // calculate the number of factors of the number
  def fac_num(n: BigInt) =
    import java.math.*
    val n_sqrt = BigInt(BigInteger(n.toString).sqrt())
    val r = (BigInt(1) to n_sqrt)
      .filter(x => n % x == 0)
      .length
    if (n_sqrt * n_sqrt == n) then 2 * r - 1 else 2 * r
  val r = ls.find(n => fac_num(n) > 500).get

def t13 =
  val nums = io.Source
    .fromFile("/Users/felix/euler-projects-test/euler13.txt")
    .getLines
    .toList
    .map(n => BigInt(n))
    .sum
    .toString
    .take(10)
    .toLong

  val res =
    io.Source
      .fromFile("/Users/felix/euler-projects-test/euler13.txt")
      .getLines
      .toList
      .map(_.take(11).toLong)
      .sum
      .toString
      .take(10)
      .toLong

def t14 =
  def collatz(n: Int, count: Int): Int =
    if (n == 1) then count + 1
    else
      val m = if (n % 2 == 0) then n / 2 else 3 * n + 1
      collatz(m, count + 1)
  val r = (1 to 100).view
    .map(x => (x, collatz(x, 0)))
    .reduceLeft((x, y) => if (x._2 > y._2) then x else y)
    ._2

def t22 =
  val s = io.Source
    .fromFile("/Users/felix/Downloads/p022_names.txt")
    .mkString
    .split(",")
    // init and tail of string, select without first and last char of string
    .map(_.init.tail)
    .sorted
    .map(s => s.map(c => c - 64).sum)
    .zipWithIndex
    .map(x => (x._1 * (x._2 + 1)))
    .sum
