import AggregateGrammar.Neighboring
import AggregateState.*
import cats.free.Free
import cats.~>
import cats.data.State

enum AggregateGrammar[T]:
  case Branch(condition: Boolean, th: () => Aggregate[T] | T, el: () => Aggregate[T] | T)
  case Repeating(initial: T, f: T => Aggregate[T] | T)
  case Neighboring(value: T) extends AggregateGrammar[Field[T]]
  case Share(value: T, f: Field[T] => Aggregate[T] | T)

type Aggregate[T] = Free[AggregateGrammar, T]

private def reduce[T](result: Aggregate[T] | T)(state: (Import, DeviceMessage)): AggregateState[T] =
  result match
    case aggregate: Aggregate[T] @unchecked =>
      val (updatedState, result) = aggregate.foldMap(aggregateCompiler).run(state).value
      State.set(updatedState).map(_ => result)
    case result => State.set(state).map(_ => result.asInstanceOf[T])

def aggregateCompiler: AggregateGrammar ~> AggregateState = new (AggregateGrammar ~> AggregateState):
  override def apply[T](fa: AggregateGrammar[T]): AggregateState[T] = fa match
    case AggregateGrammar.Branch(condition, th, el) =>
      for
        _ <- alignUpdate(s"branch/$condition")
        state <- State.get[(Import, DeviceMessage)]
        result <- reduce(if condition then th() else el())(state)
        _ <- dealignUpdate
      yield result
    case AggregateGrammar.Neighboring(value) =>
      for
        _ <- alignUpdate("neighboring")
        currentPath <- getCurrentPath
        neighborValues <- getNeighborValuesAtPath[T](currentPath)
        field = Field(value, neighborValues)
        _ <- updateDeviceMessage(currentPath, value)
        _ <- dealignUpdate
      yield field.asInstanceOf[T]
    case AggregateGrammar.Repeating(initialState, body) =>
      for
        _ <- alignUpdate("repeating")
        state <- State.get[(Import, DeviceMessage)]
        currentPath <- getCurrentPath
        previousState <- getDeviceStateOrDefault[T](currentPath, initialState)
        result <- reduce(body(previousState))(state)
        _ <- updateDeviceState[T](currentPath, result)
        _ <- dealignUpdate
      yield result
    case AggregateGrammar.Share(initialValue, body) =>
      for
        _ <- alignUpdate("share")
        state <- State.get[(Import, DeviceMessage)]
        currentPath <- getCurrentPath
        previousState <- getDeviceStateOrDefault[T](currentPath, initialValue)
        neighborValues <- getNeighborValuesAtPath[T](currentPath)
        field = Field(initialValue, neighborValues)
        result <- reduce(body(field))(state)
        _ <- updateDeviceState[T](currentPath, result)
        _ <- updateDeviceMessage(currentPath, result)
        _ <- dealignUpdate
      yield result

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
