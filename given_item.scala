package myApp.items

// The given keyword is used to define an instance of implicit value:
given timeout: Int = 10
// In Scala 3, it’s not required to name a given instance. We can directly
// assign a value without the name:
// given Int = 10

// The using keyword is used to pass an implicit parameter to a method:

case class Item(name: String, price: Double)

// define an ordering for our Item class. Since we don’t want to pass the
// ordering parameter explicitly in every function call, we’re going to create
// it as a contextual parameter by defining a given instance:
given itemOrder: Ordering[Item] = new Ordering[Item] {
  override def compare(x: Item, y: Item): Int = x.price.compare(y.price)
}
given pageLimit: Int = 6

//Anonymous Givens
//given Item = Item("Dummy", 2.0)

case class Book(name: String, year: Int, month: Int)
given bookOrder: Ordering[Book] = new Ordering[Book]:
  override def compare(x: Book, y: Book): Int =
    if x.year != y.year then x.year.compare(y.year)
    else if x.month != y.month then x.month.compare(y.month)
    else x.name.compareTo(y.name)
