package io.github.nicolasfara.multiparty

import cats.free.Free
import cats.{Monad, ~>}
import io.github.nicolasfara.multiparty.LocalProgram.LocalProcess
import io.github.nicolasfara.multiparty.Peer

object MultiParty:
  opaque infix type on[+V, -P <: Peer] = Placement[V, P]

  trait Token[V, P <: Peer]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

//  def macroprogram() {
//    val a1: F[Int on P] = placed[P] {
//
//    }
//
//    val a2: F[Int on Q] = placed[Q] {
//
//    }
//
//    val a11 = a1.await
//    val a22 = a2.await
//
//    placed[P] {
//      a11.asLocal: Int
//    }
//  }

  trait PeerScope[P <: Peer]

//  type TupleSpace <: { type Tie <: Single[Bob] & Single[Alice] }
//  type Alice <: { type Tie <: Single[TupleSpace] }
//  type Bob   <: { type Tie <: Single[TupleSpace] }
//
//  case Out[T, S <: Peer, TS <: TiedWith[S]](value: T on S) extends TupleSpaceGrammar[T on TS]
//  case In[T, S <: Peer, TS <: TiedWith[S]](pattern: String on S) extends TupleSpaceGrammar[T on S]

  enum MultiPartyGrammar[T]:
    case Placed[P <: Peer, V](value: PeerScope[P] => V) extends MultiPartyGrammar[V on P]
    case Unicast[V, From <: Peer, To <: TieToSingle[From]](value: V on From) extends MultiPartyGrammar[V on To]
    case Broadcast[V, From <: Peer, To <: TieToMultiple[From]](value: V on From) extends MultiPartyGrammar[V on To]
    case Await[V, Other <: Peer, P <: TieToSingle[Other]: PeerScope](placed: V on Other) extends MultiPartyGrammar[V]
    case FoldAwait[A, B, Other <: Peer, P <: TieToMultiple[Other]: PeerScope](placed: A on Other, zero: B)(
        f: (B, A) => B
    ) extends MultiPartyGrammar[B]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  def project: MultiPartyGrammar ~> LocalProcess = new (MultiPartyGrammar ~> LocalProcess) {
    import MultiPartyGrammar.*
    override def apply[A](fa: MultiPartyGrammar[A]): LocalProcess[A] = ???
  }
