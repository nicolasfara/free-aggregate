package io.github.nicolasfara.choreography

import io.github.nicolasfara.choreography.Choreography.*

object Examples:
  import Choreography.ChoreographyOps.*
  private type Alice <: Peer
  private type Bob <: Peer

  def simpleProtocol: Choreography[Unit] = for
    aliceMsg <- locally[Alice, String](_ => "Hi, Bob!")
    bobMsg <- comm[Alice, Bob, String](aliceMsg)
    condition <- locally[Bob, Boolean](extract => {
      val msg = extract(bobMsg)
      println(s"Bob received message: $msg")
      msg == "Hi, Bob!"
    })
    _ <- conditional(condition) {
      case true  => pure(10)
      case false => pure(20)
    }
  yield ()
