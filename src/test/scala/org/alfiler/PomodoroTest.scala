package org.alfiler

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

class PomodoroTest extends TestKit(ActorSystem("ShoppingCartActorSpec"))
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  import scala.concurrent.duration._

  override def afterAll(): Unit =
    TestKit.shutdownActorSystem(system)

  "Pomodoro" should "configure the duration" in {
    val pomodoroID = "yepe-000001"

    val pomodoro = system.actorOf(Pomodoro.props(pomodoroID))

    val duration = 20.seconds
    pomodoro ! SetTimer(duration)

    expectMsg(SettingsChanged(duration))
  }
}
