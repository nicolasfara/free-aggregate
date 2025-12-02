package io.github.nicolasfara.choreography

import io.github.nicolasfara.choreography.Choreography.*

object Examples:
  import Choreography.ChoreographyOps.*
  private type Alice <: Peer
  private type Bob <: Peer

  def simpleProtocol: Choreography[Unit] = for
    aliceMsg <- locally[Alice, String](_ => "Hi, Bob!")
    bobMsg <- comm[Alice, Bob, String](aliceMsg)
    _ <- locally[Bob, Unit](extract => {
      val msg = extract(bobMsg)
      println(s"Bob received message: $msg")
    })
  yield ()
