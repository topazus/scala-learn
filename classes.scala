package myApp.my_classes

def t1 =
  // value class
  // it must have exactly one parameter and it must have nothing inside it
  // except defs. Furthermore, no other class can extend a value class, and
  // a value class cannot redefine equals or hashCode.
  // To define a value class, make it a subclass of AnyVal, and put val before the one parameter.

  // val prefix allows the amount parameter to be accessed as a field.
  class Dollars(val amount: Int) extends AnyVal:
    override def toString = "$" + amount


