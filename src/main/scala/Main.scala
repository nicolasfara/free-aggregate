@main def hello(): Unit =
  val export1 = Map(
    "InvocationCoordinate(neighboring,0)" -> 1,
    "InvocationCoordinate(neighboring,1)" -> 10
  )
  val export2 = Map(
    "InvocationCoordinate(neighboring,0)" -> 2,
    "InvocationCoordinate(neighboring,1)" -> 20
  )
//  val importValue = Import(Map.empty, Map(1 -> export1, 2 -> export2), Stack())
  val importValue = Import(Map.empty, Map.empty, Stack())
  val ((imp, toSend), result) = aggregateProgram.foldMap(aggregateCompiler).run((importValue, Map.empty)).value
  println(imp.state)
  println(toSend)
  println(result)

def aggregateProgram = for
  f <- share(0): field =>
    for
      a <- neighboring(11)
      b <- neighboring(12)
      res = (a + b).min()
    yield res
yield f
