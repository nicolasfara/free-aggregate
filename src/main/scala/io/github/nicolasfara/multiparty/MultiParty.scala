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

  def macroprogram() {
    val a1: F[Int on P] = placed[P] {

    }

    val a2: F[Int on Q] = placed[Q] {

    }

    val a11 = a1.await
    val a22 = a2.await

    placed[P] {
      a11.asLocal: Int
    }
  }

//  type TupleSpace <: { type Tie <: Single[Bob] & Single[Alice] }
//  type Alice <: { type Tie <: Single[TupleSpace] }
//  type Bob   <: { type Tie <: Single[TupleSpace] }
//
//  case Out[T, S <: Peer, TS <: TiedWith[S]](value: T on S) extends TupleSpaceGrammar[T on TS]
//  case In[T, S <: Peer, TS <: TiedWith[S]](pattern: String on S) extends TupleSpaceGrammar[T on S]

  enum MultiPartyGrammar[T]:
    case Placed[P <: Peer, V, F[_]: Monad](value: Unwrapper[P] => V) extends MultiPartyGrammar[F[V on P]]
    case Unicast[V, From <: Peer, To <: TieToSingle[From], F[_]](value: V on From) extends MultiPartyGrammar[F[V on To]]
    case Broadcast[V, From <: Peer, To <: TieToMultiple[From], F[_]](value: V on From) extends MultiPartyGrammar[F[V on To]]
//    case Sub[V, P <: Peer](continuation: MultiPartyGrammar[V]) extends MultiPartyGrammar[V on P]
//    case Condition[V, R, P <: Peer](scrutinee: V, choice: V => MultiPartyGrammar[R]) extends MultiPartyGrammar[R on P]
//    case Fork[V, P <: Peer](continuation: MultiPartyGrammar[V]) extends MultiPartyGrammar[Token[V, P]]
//    case Join[V, P <: Peer](token: Token[V, P]) extends MultiPartyGrammar[V on P]
    case Await[V, P <: Peer, F[_]](placed: F[V on P]) extends MultiPartyGrammar[V on P]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  def project: MultiPartyGrammar ~> LocalProcess = new (MultiPartyGrammar ~> LocalProcess) {
    import MultiPartyGrammar.*
    override def apply[A](fa: MultiPartyGrammar[A]): LocalProcess[A] = fa match {
      case Placed(value)                => ???
      case Unicast(value)               => ???
      case Broadcast(value)             => ???
//      case Sub(continuation)            => ???
//      case Condition(scrutinee, choice) => ???
      case Fork(continuation)           => ???
      case Join(token)                  => ???
    }
  }
