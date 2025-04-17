
case class Field[T](local: T, neighbors: Map[Int, T])

extension[T](f: Field[T])
  def min(): T = ???
  infix def +(another: Field[T]): Field[T] = ???
