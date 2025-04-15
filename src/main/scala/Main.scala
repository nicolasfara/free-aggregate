@main def hello(): Unit =
  val export1 = Map(
    "InvocationCoordinate(neighboring,0)" -> 1,
    "InvocationCoordinate(neighboring,1)" -> 10
  )
  val export2 = Map(
    "InvocationCoordinate(neighboring,0)" -> 1,
    "InvocationCoordinate(neighboring,1)" -> 20
  )
  val importValue = Import(Map.empty, Map(1 -> export1, 2 -> export2))
  val res = msg.foldMap(aggregateCompiler).run((importValue, Stack(), Map.empty, Map.empty)).value
  println(res._2)
  println(res._1._3)

def msg =
  for
    f <- neighboring(10)
    g <- neighboring(11)
    r <- repeating(0)(x => if x < 10 then x + 1 else x)
  yield f
