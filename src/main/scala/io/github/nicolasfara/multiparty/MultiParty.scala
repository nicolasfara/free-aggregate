package io.github.nicolasfara.multiparty

import cats.free.Free
import cats.~>
import io.github.nicolasfara.multiparty.LocalProgram.LocalProcess
import io.github.nicolasfara.multiparty.Peer

object MultiParty:
  opaque infix type on[+V, -P <: Peer] = Placement[V, P]

  trait Token[V, P <: Peer]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  sealed trait Unwrapper[P <: Peer]:
    def apply[V](placed: V on P): V

  enum MultiPartyGrammar[T]:
    case Placed[P <: Peer, V](value: Unwrapper[P] => V) extends MultiPartyGrammar[V on P]
    case Unicast[V, From <: Peer, To <: TieToSingle[From]](value: V on From) extends MultiPartyGrammar[V on To]
    case Broadcast[V, From <: Peer, To <: TieToMultiple[From]](value: V on From) extends MultiPartyGrammar[V on To]
    case Sub[V, P <: Peer](continuation: MultiPartyGrammar[V]) extends MultiPartyGrammar[V on P]
    case Condition[V, R, P <: Peer](scrutinee: V, choice: V => MultiPartyGrammar[R]) extends MultiPartyGrammar[R on P]
    case Fork[V, P <: Peer](continuation: MultiPartyGrammar[V]) extends MultiPartyGrammar[Token[V, P]]
    case Join[V, P <: Peer](token: Token[V, P]) extends MultiPartyGrammar[V on P]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  def project: MultiPartyGrammar ~> LocalProcess = new (MultiPartyGrammar ~> LocalProcess) {
    import MultiPartyGrammar.*
    override def apply[A](fa: MultiPartyGrammar[A]): LocalProcess[A] = fa match {
      case Placed(value)                => ???
      case Unicast(value)               => ???
      case Broadcast(value)             => ???
      case Sub(continuation)            => ???
      case Condition(scrutinee, choice) => ???
      case Fork(continuation)           => ???
      case Join(token)                  => ???
    }
  }
