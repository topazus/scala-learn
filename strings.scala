package myApp.strings
def str1 =
  // string split
// does not return trailing empty strings by default.
  val s = "1,2,3,"
  var res = s.split(",").toList
  println(res)
  //
  res = s.split(",", -1).toList
  println(res)
