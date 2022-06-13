package windymelt

import com.github.nscala_time.time.Imports._
import cats._
import cats.implicits._

object Hello extends App {
  val testInA = Seq(
    Action(1, 123, Start(SuperPremium), DateTime.parse("2022-06-01T00:00:00")),
    Action(2, 123, Change(Premium), DateTime.parse("2022-07-01T00:00:00")),
    Action(3, 123, Exit, DateTime.parse("2022-08-01T00:00:00")),
    Action(4, 123, Start(Premium), DateTime.parse("2022-09-01T00:00:00")),
    Action(5, 123, Change(SuperPremium), DateTime.parse("2022-10-01T00:00:00")),
    Action(6, 123, Change(Premium), DateTime.parse("2022-11-01T00:00:00")),
    Action(7, 123, Exit, DateTime.parse("2022-12-01T00:00:00"))
  )

  val testInB = Seq(
    Action(8, 456, Start(SuperPremium), DateTime.parse("2022-07-01T00:00:00")),
    Action(9, 456, Change(Premium), DateTime.parse("2022-08-01T00:00:00")),
    // 毎月Actionが起こるわけではない。
    Action(10, 456, Exit, DateTime.parse("2022-12-01T00:00:00"))
  )

  val testIn = testInA |+| testInB
  val from = DateTime.parse("2022-01-01T00:00:00")
  val until = DateTime.parse("2023-02-01T00:00:00")
  val cohort: Types.TransitionCountWithTiming =
    Cohort.calcCohort(testIn)( /* use default */ )(from, until).combineAll
  val cohortForSuperPremiumToPremium = Cohort.transformCohortForPrint(cohort)(
    (Start(SuperPremium), Change(Premium))
  ).get
  print(Cohort.showCohort(cohortForSuperPremiumToPremium, from, until))
}
