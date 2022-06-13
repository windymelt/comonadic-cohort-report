package windymelt

import Types._
import com.github.nscala_time.time.Imports._
import cats._
import cats.implicits._
import cats.data.NonEmptyList

object Cohort {
  val detectMultiWithNoSameVec: Action => Action => TransitionCountWithTiming =
    (s: Action) =>
      (x: Action) =>
        (s, x) match {
          case (s, x) if s.vec == x.vec => Map.empty // Exclude same transition
          case (s, x) =>
            Map[(Vec, Vec), Map[(DateTime, DateTime), Count]](
              (
                (s.vec, x.vec),
                Map[(DateTime, DateTime), Count](
                  ((s.occurredAt, x.occurredAt), 1)
                )
              )
            )
        }

  private def getDesiredMomentOpt[F: Monoid](
      detector: Action => Action => F
  )(xs: ByMonth[Option[Action]]): Option[Seq[F]] = {
    val maybeNel = NonEmptyList.fromList(xs)
    maybeNel map { ys: NonEmptyList[Option[Action]] =>
      val nested = ys coflatMap { (zs: NonEmptyList[Option[Action]]) =>
        (Some(zs.head), zs.get(1)) mapN { (fst, snd) =>
          ((fst, snd) mapN { (f, s) => detector(f)(s) }).orEmpty
        }
      }
      nested.toList.flatten
    }
  }

  def calcCohort[F: Monoid](in: Seq[Action])(
      detector: Action => Action => F = this.detectMultiWithNoSameVec
  )(from: DateTime, until: DateTime) = Util.splitAndCombine(in.toList)(_.user) {
    as =>
      getDesiredMomentOpt(detector)(
        Util.interExtrapolateMonth(Util.inToByMonth(as))(from, until)
      ).orEmpty.toList
  }

  def transformCohortForPrint(
      cohort: TransitionCountWithTiming
  )(desiredPair: (Vec, Vec)) = {
    cohort.get(desiredPair).map { m =>
      // Keyがかぶると値が上書きされてしまうので、一度各要素ごとにMapを作らせてからreduceすることで合体させる
      m map { case ((s, t), v) =>
        Map(s.to(t).toPeriod.getMonths() -> Map((s, v)))
      } reduce(_ |+| _)
    }
  }

  def showCohort(
      cohort: CohortPerTransition,
      from: DateTime,
      until: DateTime
  ): String = {
    // そんで、1行は経過した月数に相当するので、最大 until - from の月数にあたるはず。
    // 色々あって月数を数えられないので、30日で割って四捨五入したらだいたいいいでしょということにする。
    val durationMonth =
      Math.round((from to until).toDuration().getStandardDays() / 30.0)

    val fromToUntilMonthsRange =
      from.to(until).toDuration().getStandardDays() / 30

    val header = (0 to fromToUntilMonthsRange.toInt) map { diff =>
      from.plusMonths(diff).toString("yyyy-MM")
    } prepended ("diff") mkString ("\t")

    val body = (0 to fromToUntilMonthsRange.toInt) map { diff =>
      // diffはnヶ月目にあたる情報。
      // diffヶ月目の情報を取り出す。なければ空にする
      val cohortForThisDiff = cohort.get(diff).getOrElse(Map())
      // diffヶ月目の情報を、月別に取り出して表示する
      val lis = (0 to fromToUntilMonthsRange.toInt) map { diff2 =>
        val mo = from.plusMonths(diff2)
        cohortForThisDiff.get(mo).orEmpty
      }
      lis.prepended(s"${diff}ヶ月目").mkString("\t")
    } mkString ("\n")

    header + "\n" + body + "\n"
  }
}
