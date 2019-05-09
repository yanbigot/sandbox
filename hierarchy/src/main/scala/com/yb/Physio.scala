package com.yb

import akka.actor.{Actor, ActorRef, ActorSystem}

class Physio(message: String, from: ActorRef) extends Actor {
  def receive: Receive = {
    case _ => println("youpi")
  }
}
