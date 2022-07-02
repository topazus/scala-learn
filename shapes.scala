package myApp.shapes

case class Point(x: Double = 0.0, y: Double = 0.0) // <1>

abstract class Shape(): // <2>
  /** Draw the shape.
    * @param f
    *   is a function to which the shape will pass a string version of itself to
    *   be rendered.
    */
  def draw(f: String => Unit): Unit = f(s"draw: $this")

case class Circle(center: Point, radius: Double) extends Shape

case class Rectangle(lowerLeft: Point, height: Double, width: Double)
    extends Shape

case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape

trait ToJSON[T]:
  extension (t: T) def toJSON(name: String = "", level: Int = 0): String

  protected val indent = "  "
  protected def indentation(level: Int): (String, String) =
    (indent * level, indent * (level + 1))
  protected def handleName(name: String): String =
    if name.length > 0 then s""""$name": """ else ""

// given keyword declares an instance of the type class
given ToJSON[Point] with
  extension (point: Point)
    def toJSON(name: String = "", level: Int = 0): String =
      val (outdent, indent) = indentation(level)
      s"""${handleName(name)}{
        |$indent"x": "${point.x}",
        |${indent}"y": "${point.y}"
        |$outdent}""".stripMargin

given ToJSON[Circle] with // <2>
  extension (circle: Circle)
    def toJSON(name: String = "", level: Int = 0): String =
      val (outdent, indent) = indentation(level)
      s"""${handleName(name)}{
        |${indent}${circle.center.toJSON("center", level + 1)},
        |${indent}"radius": ${circle.radius}
        |$outdent}""".stripMargin

given ToJSON[Rectangle] with
  extension (rect: Rectangle)
    def toJSON(name: String = "", level: Int = 0): String =
      val (outdent, indent) = indentation(level)
      s"""${handleName(name)}{
        |${indent}${rect.lowerLeft.toJSON("lowerLeft", level + 1)},
        |${indent}"height":    ${rect.height}
        |${indent}"width":     ${rect.width}
        |$outdent}""".stripMargin

given ToJSON[Triangle] with
  extension (tri: Triangle)
    def toJSON(name: String = "", level: Int = 0): String =
      val (outdent, indent) = indentation(level)
      s"""${handleName(name)}{
        |${indent}${tri.point1.toJSON("point1", level + 1)},
        |${indent}${tri.point2.toJSON("point2", level + 1)},
        |${indent}${tri.point3.toJSON("point3", level + 1)},
        |$outdent}""".stripMargin

given ToJSON[Shape] with
  extension (shape: Shape)
    def toJSON(name: String = "", level: Int = 0): String =
      shape match
        case c: Circle =>
          summon[ToJSON[Circle]].toJSON(c)(name, level)
        case r: Rectangle =>
          summon[ToJSON[Rectangle]].toJSON(r)(name, level)
        case t: Triangle =>
          summon[ToJSON[Triangle]].toJSON(t)(name, level)

def TryJSONTypeClasses =
  println(s"summon[ToJSON[Point]] = ${summon[ToJSON[Point]]}") // <1>
  println(s"summon[ToJSON[Circle]] = ${summon[ToJSON[Circle]]}")
  println(Circle(Point(1.0, 2.0), 1.0).toJSON("circle", 0))
  println(Rectangle(Point(2.0, 3.0), 2, 5).toJSON("rectangle", 0))
  println(
    Triangle(Point(0.0, 0.0), Point(2.0, 0.0), Point(1.0, 2.0))
      .toJSON("triangle", 0)
  )
def t2 =
  val c = Circle(Point(1.0, 2.0), 1.0)
  val r = Rectangle(Point(2.0, 3.0), 2, 5)
  val t = Triangle(Point(0.0, 0.0), Point(2.0, 0.0), Point(1.0, 2.0))
  println("==== Use shape.toJSON:")
  Seq(c, r, t).foreach(s => println(s.toJSON("shape", 0)))
  println("==== call toJSON on each shape explicitly:")
  println(c.toJSON("circle", 0))
  println(r.toJSON("rectangle", 0))
  println(t.toJSON("triangle", 0))
