package myApp.hello

/*
import scala.util.*                   // Instead of _
import scala.concurrent.{given, *}    // Everything in concurrent, including givens
import java.util.Queue as JQueue      // "as" replaces => and no braces needed for one import
import java.util.{HashMap as _, *}    // Need {} for a list. Still use _ to hide HashMap
 */

import java.io.File

@main def helloWorld =
  myApp.shapes.t2

def read_file =
  // open file
  val f = io.Source.fromFile("/Users/felix/scala-learn/my-app/fedora.txt")
  // get lines
  val lines = f.getLines.toList
    .map(str =>
      var t = str.split("=", -1)
      val k = t(0)
      val v = t(1).replaceAll("\"", "")
      (k, v)
    )
  // close file
  f.close
  println(lines)
