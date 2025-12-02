package io.github.nicolasfara.choreography

import cats.free.Free
import cats.{Id, ~>}

object Choreography:
  private type Ref
  private enum Placed[+V, -P <: Peer]:
    case Local(ref: Ref, value: V) extends Placed[V, P]
    case Remote(ref: Ref) extends Placed[V, P]

  trait Peer
  opaque infix type on[+V, -P <: Peer] = Placed[V, P]

  sealed trait Unwrapper[P <: Peer]:
    def apply[V](placed: V on P): V

  sealed trait ChoreographyGrammar[T]
  case class Locally[P <: Peer, V](body: Unwrapper[P] => V) extends ChoreographyGrammar[V on P]
  case class Comm[From <: Peer, To <: Peer, V](value: V on From) extends ChoreographyGrammar[V on To]
  case class Condition[P <: Peer, A](condition: Boolean on P, choice: () => Choreography[A])
      extends ChoreographyGrammar[A]

  type Choreography[T] = Free[ChoreographyGrammar, T]
  object ChoreographyOps:
    inline def locally[P <: Peer, V](body: Unwrapper[P] => V): Choreography[V on P] =
      Free.liftF(Locally[P, V](body))
    inline def comm[From <: Peer, To <: Peer, V](value: V on From): Choreography[V on To] =
      Free.liftF(Comm[From, To, V](value))
    inline def conditional[P <: Peer, A](condition: Boolean on P)(choice: => Choreography[A]): Choreography[A] =
      Free.liftF(Condition[P, A](condition, () => choice))

  object ChoreographyInterpreter:
    def interpreter: ChoreographyGrammar ~> Id = ???
    def run[A](choreography: Choreography[A]): A =
      choreography.foldMap(interpreter)
