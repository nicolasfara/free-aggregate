case class Field[T](local: T, neighbors: Map[Int, T])

extension [T: Numeric as n](f: Field[T])
  /** Returns the minimum value of the field, which is the minimum of the local value and the minimum of the neighbors.
    * @return
    */
  def min(): T =
    val localMin = f.local
    val neighborMin = f.neighbors.values.minOption
    n.min(localMin, neighborMin.getOrElse(localMin))

  /** Returns the maximum value of the field, which is the maximum of the local value and the maximum of the neighbors.
    * @return
    */
  def max: T =
    val localMax = f.local
    val neighborMax = f.neighbors.values.maxOption
    n.max(localMax, neighborMax.getOrElse(localMax))

  /** Returns the sum of two fields.
    * @param another
    *   the other field to sum
    * @return
    *   the sum of the two fields
    */
  infix def +(another: Field[T]): Field[T] =
    val containsAllKeys = f.neighbors.keys.forall(another.neighbors.keySet.contains(_))
    require(containsAllKeys, s"field not aligned: ${f.neighbors.keySet -- another.neighbors.keySet}")
    Field(
      n.plus(f.local, another.local),
      f.neighbors.map { case (k, v) => k -> n.plus(v, another.neighbors(k)) }
    )
