def t1=
  val lazyList = 1 #:: 2 #:: 3 #:: LazyList.empty

def t2=
  import scala.collection.immutable.ArraySeq
  val arr = ArraySeq(1, 2, 3)
  val arr2 = arr :+ 4
  val a=arr2(0)
  val arr3=arr2.updated(2, 4)
  val arr4=arr2.prepended(0)
  val arr5=arr2.appended(5)
