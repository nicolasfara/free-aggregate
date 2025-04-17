import scala.collection.mutable

case class InvocationCoordinate(token: String, count: Int)

class Stack:
  opaque type Path[T] = List[T]
  private val trace: mutable.Map[Path[InvocationCoordinate], Int] = mutable.Map.empty
  private val stack: mutable.Stack[InvocationCoordinate] = mutable.Stack.empty

  def currentPath: IndexedSeq[InvocationCoordinate] = stack.toIndexedSeq

  def align(token: String): Unit =
    val next = trace.get(stack.toList).map(_ + 1).getOrElse(0)
    trace.update(stack.toList, next)
    stack.push(InvocationCoordinate(token, next))

  def dealign(): Unit = if stack.nonEmpty then stack.pop() else throw new IllegalStateException("Stack is empty")
