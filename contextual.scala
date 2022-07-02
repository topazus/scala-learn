package myApp.implicit_conversions

// implicit conversion from Street to String.
// defining a given instance of type Conversion[Street, String], which is a
// subtype of the function type, Street => String. It is defined like this:
//   abstract class Conversion[-T, +U] extends (T => U):
//     def apply(x: T): U
// Because trait Conversion has a single abstract method, you can often use
// a SAM function literal to define an instance.
def implicit_conversions1=
  import language.implicitConversions
  case class Street(value: String)
  val street = Street("123 Main St")
  given street2String: Conversion[Street,String]= s=>s.value
  // given street2String: Conversion[Street,String]= _.value


def implicit_conversions2=
  import language.implicitConversions
  case class Dollars(amount: Double):
    override def toString = f"$$$amount%.2f"
  case class Percentage(amount: Double):
    override def toString = f"${(amount * 100.0)}%.2f%%"
  case class Salary(gross: Dollars, taxes: Percentage):
    def net: Dollars = Dollars(gross.amount * (1.0 - taxes.amount))

  // declare an implicit conversion
  given double2dollars: Conversion[Double, Dollars] = d => Dollars(d)
  given double2percentage: Conversion[Double, Percentage] with
    def apply(x: Double): Percentage = Percentage(x)

  val salary = Salary(100_000.0, 0.20)

  println(s"salary: $salary. Net pay: ${salary.net}")
