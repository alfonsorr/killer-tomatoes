package org.alfiler

import java.time.Instant

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestActors, TestKit, TestProbe}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

import scala.concurrent.Await

class PomodoroTest extends TestKit(ActorSystem("ShoppingCartActorSpec"))
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender {

  import akka.pattern._

  import scala.concurrent.duration._

  override def afterAll(): Unit =
    TestKit.shutdownActorSystem(system)

  "Pomodoro" should "configure the duration" in {
    val pomodoroID = "yepe-000001"

    val echo = system.actorOf(TestActors.echoActorProps)

    val pomodoro = system.actorOf(Pomodoro.props(pomodoroID, echo))

    val duration = 20.seconds
    pomodoro ! SetTimer(duration)

    expectMsg(SettingsChanged(duration))
  }

  it should "do a full circle" in {
    val pomodoroID = "yepe-000002"
    implicit val t:Timeout = 1.second

    val probe = TestProbe()
    val pomodoro = system.actorOf(Pomodoro.props(pomodoroID, probe.ref))


    val DURATION = 20.seconds
    pomodoro ! SetTimer(DURATION)

    expectMsg(SettingsChanged(DURATION))
    pomodoro ! Start
    expectMsgPF(1.second){case Started(DURATION,endAt) => endAt}
    probe.expectMsg(DURATION+1.seconds, Ended)
  }
}
