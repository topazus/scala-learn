package myApp.usingExample
//import myApp.items.given_Item

// One of Scala’s most famous features is implicits, a mechanism used for many
// idioms, like simulating extension methods (defined below), passing arguments
// implicitly to methods (rather than explicitly), constraining allowed types,
// and other idioms. Collectively, these idioms are contextual abstractions.

// If the compiler does not find an applicable given in lexical scope, it will
// as a second step look for given definitions in the companion object of all
// the types involved in the context parameter type.
//
import myApp.items.{itemOrder, pageLimit, Item}

import myApp.items.{Book, bookOrder}
val shoppingCart = List(
  Item("PanCake", 4),
  Item("Coke", 1),
  Item("Pizza", 5),
  Item("Burger", 3)
)

// In Scala 3, this feature is provided through the using clause. The compiler
// looks for parameters marked with the using clause and replaces them with a
// given instance of a compatible type available within the scope.

// Context parameters are those that are defined in a using clause.
def listItems(
    products: Seq[Item]
)(using ordering: Ordering[Item])(using limit: Int) = {
  products.sorted.take(limit)
}

def sort_books(books: Seq[Book])(using ordering: Ordering[Book]) =
  books.sorted
// Context parameters can help here since they can prevent calling repetitive arguments
// ord is a context parameter introduced with a using clause.
def max[T](x: T, y: T)(using ord: Ordering[T]): T =
  if ord.compare(x, y) < 0 then y else x

def usingEx =
  val books = List(
    Book("a", 2010, 2),
    Book("b", 2010, 9),
    Book("c", 2014, 7)
  )
  println(listItems(shoppingCart))
  println(sort_books(books))
