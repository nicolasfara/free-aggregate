import cats.data.State

type DeviceState = Map[String, Any]
type ValueTree = Map[String, Any]
type DeviceMessage = Map[String, Any]
type NeighborMessages = Map[Int, ValueTree]

case class Import(state: DeviceState, messages: NeighborMessages, stack: Stack)
object Import:
  def empty: Import = Import(Map.empty, Map.empty, Stack())

type AggregateState[T] = State[(Import, DeviceMessage), T]
object AggregateState:
  def alignUpdate(token: String): State[(Import, DeviceMessage), Unit] = State.modify:
    case (Import(state, messages, stack), deviceMessage) =>
      val _ = stack.align(token)
      (Import(state, messages, stack), deviceMessage)
      
  def dealignUpdate: State[(Import, DeviceMessage), Unit] = State.modify:
    case (Import(state, messages, stack), deviceMessage) =>
      val _ = stack.dealign()
      (Import(state, messages, stack), deviceMessage)
      
  def getCurrentPath: State[(Import, DeviceMessage), String] = State.inspect:
    case (Import(state, messages, stack), deviceMessage) => stack.currentPath.mkString("/")
    
  def getNeighborValuesAtPath[V](path: String): State[(Import, DeviceMessage), Map[Int, V]] = State.inspect:
    case (Import(state, messages, stack), deviceMessage) =>
      messages.collect {
        case (id, valueTree) if valueTree.contains(path) => id -> valueTree(path).asInstanceOf[V]
      }
      
  def updateDeviceMessage[V](path: String, value: V): State[(Import, DeviceMessage), Unit] = State.modify:
    case (Import(state, messages, stack), deviceMessage) =>
      val newDeviceMessage = deviceMessage.updated(path, value)
      (Import(state, messages, stack), newDeviceMessage)
  
  def updateDeviceState[V](path: String, value: V): State[(Import, DeviceMessage), Unit] = State.modify:
    case (Import(state, messages, stack), deviceMessage) =>
      val newState = state.updated(path, value)
      (Import(newState, messages, stack), deviceMessage)
      
  def getDeviceStateOrDefault[V](path: String, default: V): State[(Import, DeviceMessage), V] = State.inspect:
    case (Import(state, messages, stack), deviceMessage) =>
      state.get(path).map(_.asInstanceOf[V]).getOrElse(default)