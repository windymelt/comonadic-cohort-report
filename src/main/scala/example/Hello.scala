package example

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports._ // for DateTime

// ADTs
sealed trait Plan
case object Premium extends Plan
case object SuperPremium extends Plan

// ADTs
sealed trait Vec
case class Start(plan: Plan) extends Vec
case class Change(plan: Plan) extends Vec
case object Exit extends Vec

object Hello extends App {
  // Alias
  type Id = Int
  type Count = Int

  case class Action(id: Id, user: Id, vec: Vec, occurredAt: DateTime)
  type In = Seq[Action]

  type ~>[K, V] = Map[K, V]
  type Out = DateTime ~> Seq[Count]

// Test Data
  val testIn: In = Seq(
    Action(1, 123, Start(SuperPremium), DateTime.parse("2022-06-01T00:00:00")),
    Action(2, 123, Change(Premium), DateTime.parse("2022-07-01T00:00:00")),
    Action(3, 123, Exit, DateTime.parse("2022-08-01T00:00:00"))
  )

// いったん決め打ちしている
  val detect = (s: Vec) =>
    (x: Vec) =>
      (s, x) match {
        case (Change(Premium), Exit) => true
        case otherwise               => false
      }

// helper
  implicit val actionOrdering: Ordering[Action] = Ordering.by(_.occurredAt)

// Data structure
  type ByMonth[A] = List[A]
  val inToByMonth: In => ByMonth[Action] = (in: In) =>
    in
      .groupBy(a => (a.occurredAt.year, a.occurredAt.month))
      .map { case (k, v) => k -> v.sorted.last }
      .values
      .toList
      .sorted

  inToByMonth(testIn)

  def getStartExitMoment(xs: ByMonth[Action]): Option[Seq[Boolean]] = {
    val maybeNel = NonEmptyList.fromList(xs)
    maybeNel map { ys: NonEmptyList[Action] =>
      val nested = ys coflatMap { (zs: NonEmptyList[Action]) =>
        (Some(zs.head), zs.get(1)) mapN { (fst, snd) =>
          detect(fst.vec)(snd.vec)
        }
      }
      nested.toList.flatten
    }
  }

  getStartExitMoment(inToByMonth(testIn))
}
