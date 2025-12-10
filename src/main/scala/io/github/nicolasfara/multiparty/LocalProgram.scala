package io.github.nicolasfara.multiparty

import cats.free.Free
import cats.{Id, ~>}
import io.github.nicolasfara.multiparty.MultiParty.on

object LocalProgram:
  enum LocalGrammar[T]:
    case Send[From <: Peer, To <: Peer, V](value: V on From) extends LocalGrammar[Unit]
    case Receive[From <: Peer, To <: Peer, V](ref: String) extends LocalGrammar[V]
    case Suspend[U](token: String, continuation: LocalProcess[U]) extends LocalGrammar[U]
    case Branch[A, B](scrutinee: A, choice: A => LocalProcess[B]) extends LocalGrammar[B]
    case Choice[U](option: PartialFunction[String, LocalProcess[U]]) extends LocalGrammar[U]
    case LocalEnd extends LocalGrammar[Nothing]

  type LocalProcess[T] = Free[LocalGrammar, T]

  def interpreter: LocalGrammar ~> Id = new (LocalGrammar ~> Id):
    import LocalGrammar.*
    override def apply[A](fa: LocalGrammar[A]): Id[A] = fa match
      case Send(value)                  => ???
      case Receive(ref)                 => ???
      case Suspend(token, continuation) => ???
      case Branch(scrutinee, choice)    => ???
      case Choice(option)               => ???
      case LocalEnd                     => ???
