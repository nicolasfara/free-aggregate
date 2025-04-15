import AggregateGrammar.Neighboring
import cats.free.Free
import cats.~>
import cats.data.State

type ValueTree = Map[String, Any]
type Path[T] = List[T]
type Export = Map[String, Any]
type ComputationalState = Map[String, Any]

case class Import(state: ComputationalState, messages: Map[Int, ValueTree])

object Import:
  def empty: Import = Import(Map.empty, Map.empty)

case class Field[T](local: T, neighbors: Map[Int, T])

extension[T](f: Field[T])
  def min(): T = ???
  infix def +(another: Field[T]): Field[T] = ???

enum AggregateGrammar[T]:
  case Branch(condition: Boolean, th: () => Aggregate[T] | T, el: () => Aggregate[T] | T)
  case Repeating(initial: T, f: T => Aggregate[T] | T)
  case Neighboring(value: T) extends AggregateGrammar[Field[T]]
  case Share(value: T, f: Field[T] => Aggregate[T] | T)

type Aggregate[T] = Free[AggregateGrammar, T]
type AggregateState[T] = State[(Import, Stack, Export, ComputationalState), T]

def reduce[T](result: Aggregate[T] | T)(state: (Import, Stack, Export, ComputationalState)): ((Import, Stack, Export, ComputationalState), T) =
  result match
    case agg: Aggregate[T] @unchecked => agg.foldMap(aggregateCompiler).run(state).value
    case r => (state, r.asInstanceOf[T])

def aggregateCompiler: AggregateGrammar ~> AggregateState = new (AggregateGrammar ~> AggregateState):
  override def apply[T](fa: AggregateGrammar[T]): AggregateState[T] = fa match
    case AggregateGrammar.Branch(condition, th, el) =>
      for
        (imp, stack, exp, cs) <- State.get[(Import, Stack, Export, ComputationalState)]
        _ = stack.align(s"branch/$condition")
        (newState, result) = reduce(if condition then th() else el())(imp, stack, exp, cs)
        _ <- State.set(newState)
      yield result
    case AggregateGrammar.Neighboring(value) =>
      for
        (importValue, stack, exportValue, state) <- State.get[(Import, Stack, Export, ComputationalState)]
        _ = stack.align("neighboring")
        currentPath = stack.currentPath.mkString
        neighborValues = importValue.messages
          .filter(_._2.contains(currentPath))
          .map((id, valueTree) => id -> valueTree(currentPath).asInstanceOf[T])
        field = Field(value, neighborValues)
        _ = stack.dealign()
        result = field.asInstanceOf[T]
        _ <- State.set((importValue, stack, exportValue.updated(currentPath, value), state)) // Update the import with the new field
      yield result
    case AggregateGrammar.Repeating(initialState, body) =>
      for
        (importValue, stack, exportValue, state) <- State.get[(Import, Stack, Export, ComputationalState)]
        _ = stack.align("repeating")
        currentPath = stack.currentPath.mkString
        currentState = importValue.state.getOrElse(currentPath, initialState).asInstanceOf[T]
        (newState, result) = reduce(body(currentState))(importValue, stack, exportValue, state)
        _ <- State.set(newState)
        (imp, stack, e, cs) <- State.get[(Import, Stack, Export, ComputationalState)]
        newState = cs.updated(currentPath, result)
        _ = stack.dealign()
        _ <- State.set((imp, stack, e, newState)) // Update the import with the new state
      yield result
    case AggregateGrammar.Share(initialValue, body) =>
      for
        (importValue, stack, exportValue, state) <- State.get[(Import, Stack, Export, ComputationalState)]
      yield ???

def branch[T](condition: Boolean)(th: => T)(el: => T): Aggregate[T] =
  Free.liftF(AggregateGrammar.Branch(condition, () => th, () => el))

def repeating[T](initial: T)(f: T => Aggregate[T] | T): Aggregate[T] =
  Free.liftF(AggregateGrammar.Repeating(initial, f))

def neighboring[T](value: T): Aggregate[Field[T]] =
  Free.liftF(AggregateGrammar.Neighboring(value))

def share[T](value: T)(f: Field[T] => Aggregate[T] | T): Aggregate[T] =
  Free.liftF(AggregateGrammar.Share(value, f))

//def distances(): Aggregate[Field[Double]] = ???
//
//def gradient(source: Boolean): Aggregate[Double] =
//  for
//    sensedDist <- distances()
//    dist <- share(Double.PositiveInfinity)(f => if source then 0.0 else (sensedDist + f).min())
//  yield dist
