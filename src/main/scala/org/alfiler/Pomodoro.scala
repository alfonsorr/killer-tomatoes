package org.alfiler

import java.time.Instant

import akka.persistence.{PersistentActor, RecoveryCompleted}

import scala.concurrent.duration.Duration
import java.time.{Duration => JavaDuration}

import akka.actor.Cancellable

trait PomodoroCmd

object Start extends PomodoroCmd

case class SetTimer(duration: Duration) extends PomodoroCmd

object End extends PomodoroCmd

object Cancel extends PomodoroCmd

trait PomodoroEvent

case class Started(duration: Duration, endAt: Instant) extends PomodoroEvent

object Ended extends PomodoroEvent

object Canceled extends PomodoroEvent

case class SettingsChanged(duration: Duration) extends PomodoroEvent

object CantStart extends PomodoroEvent

trait State

case class InPomodoro(duration: Duration, endAt: Instant) extends State

case object Stopped extends State

class Pomodoro(val persistenceId: String) extends PersistentActor {
  import cats.syntax.option._

  import scala.concurrent.duration._

  var duration: Duration = 25.minutes
  var state: State = Stopped
  var launcher:Option[Cancellable] = None

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
      case (InPomodoro(_, _), Ended || Canceled) => state = Stopped
      case (Stopped, SettingsChanged(newDuration)) if newDuration != duration => duration = newDuration
    }
  }

  override def receiveRecover: Receive = {
    case evnt: PomodoroEvent => updateState(evnt)
    case RecoveryCompleted => state match {
      case Started(_, endAt) =>
        launcher = context.system.scheduler.scheduleOnce(secondsTillEndOfPomodoro(endAt), self, End).some
    }
  }

  override def receiveCommand: Receive = {
    case Start => state match {
      //case Started()
    }
    case End =>
    case Cancel =>
  }

}
