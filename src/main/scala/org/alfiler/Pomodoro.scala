package org.alfiler

import java.time.Instant

import akka.persistence.{PersistentActor, RecoveryCompleted}

import scala.concurrent.duration.Duration
import java.time.{Duration => JavaDuration}

import akka.actor.{ActorRef, Cancellable, Props}

trait PomodoroCmd

case object Start extends PomodoroCmd

case class SetTimer(duration: Duration) extends PomodoroCmd

case object End extends PomodoroCmd

case object Cancel extends PomodoroCmd

trait PomodoroEvent

case class Started(duration: Duration, endAt: Instant) extends PomodoroEvent

case object Ended extends PomodoroEvent

case object Canceled extends PomodoroEvent

case class SettingsChanged(duration: Duration) extends PomodoroEvent

case object CantStart extends PomodoroEvent

trait State

case class InPomodoro(duration: Duration, endAt: Instant) extends State

case object Stopped extends State

object Pomodoro {
  def props(id:String, notif:ActorRef):Props = Props(new Pomodoro(id, notif))
}

class Pomodoro(val persistenceId: String, notif:ActorRef) extends PersistentActor {
  import cats.syntax.option._

  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  var duration: Duration = 25.minutes
  var state: State = Stopped
  var launcher:Option[Cancellable] = None

  private def instantToEnd(duration: Duration):Instant = {
    val now = Instant.now()
    val durationinJava = JavaDuration.ofSeconds(duration.toSeconds)
    now.plus(durationinJava)
  }

  private def secondsTillEndOfPomodoro(endAt: Instant): FiniteDuration = {
    val secondsToEnd = JavaDuration.between(Instant.now(), endAt).getSeconds.seconds
    if (secondsToEnd <= 0.seconds)
      1.seconds
    else
      secondsToEnd
  }

  def updateState(evnt: PomodoroEvent): Unit = {
    (state, evnt) match {
      case (Stopped, Started(pomodoroDuration, endAt)) => state = InPomodoro(pomodoroDuration, endAt)
      case (InPomodoro(_, _), Ended | Canceled) => state = Stopped
      case (Stopped, SettingsChanged(newDuration)) => duration = newDuration
    }
  }

  override def receiveRecover: Receive = {
    case evnt: PomodoroEvent => updateState(evnt)
    case RecoveryCompleted => startScheduler.orElse(doNothing)(state)
  }

  val startScheduler:PartialFunction[State,Unit] = {
    case InPomodoro(_, endAt) =>
      launcher = context.system.scheduler.scheduleOnce(secondsTillEndOfPomodoro(endAt), self, End).some
  }
  val doNothing:PartialFunction[State,Unit] = {
    case Stopped =>
  }


  override def receiveCommand: Receive = {
    case Start => state match {
      case _: Started =>
      case Stopped => persist(Started(duration, instantToEnd(duration))) { event =>
        updateState(event)
        startScheduler(state)
        sender() ! event
      }
    }
    case End => state match {
      case InPomodoro(_,endesAt)  => persist(Ended) (ev => {
        updateState(ev)
        notif ! Ended
      })
      case _:InPomodoro | Stopped => println("ended when cant be ended")
    }
    case Cancel => persist(Canceled)(ev => updateState(ev))
    case SetTimer(newDuration) => state match {
      case Stopped => persist(SettingsChanged(newDuration)){
        e =>
          updateState(e)
          sender() ! e
      }
    }
  }

}
