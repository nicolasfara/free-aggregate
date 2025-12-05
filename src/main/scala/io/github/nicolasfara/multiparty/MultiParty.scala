package io.github.nicolasfara.multiparty

import cats.free.Free

object MultiParty:
  trait Peer
  opaque infix type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  sealed trait MultiPartyGrammar[T]
  case class Pure[P <: Peer, V](value: V) extends MultiPartyGrammar[V on P]
  case class Inspect[P <: Peer, V](placed: V on P) extends MultiPartyGrammar[V]
  case class Comm[From <: Peer, To <: Peer, V](value: V on From) extends MultiPartyGrammar[V on To]
  case class Condition[P <: Peer, A, B](scrutinee: A on P, choice: A => MultiParty[B]) extends MultiPartyGrammar[B]
  object End extends MultiPartyGrammar[Nothing]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  sealed trait LocalGrammar[T]
  case class Send[From <: Peer, To <: Peer, V](value: V on From) extends LocalGrammar[Unit]
  case class Receive[From <: Peer, To <: Peer, V](ref: String) extends LocalGrammar[V]
  case class Idle[T](token: String, continuation: LocalProcess[T]) extends LocalGrammar[T]
  case class Branch[A, B](scrutinee: A, choice: A => LocalProcess[B]) extends LocalGrammar[B]
  case class Choice[T](option: PartialFunction[String, LocalProcess[T]]) extends LocalGrammar[T]
  object LocalEnd extends LocalGrammar[Nothing]

  type LocalProcess[T] = Free[LocalGrammar, T]
