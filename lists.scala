
def t1=
  val a=((1 to 50).toList.view.map(x=>x*2)
  .map(x=>x.toString)).to(List)

def t2=
  val a=Vector(1 to 10: _*)
  val b=(1 to 10).toVector
