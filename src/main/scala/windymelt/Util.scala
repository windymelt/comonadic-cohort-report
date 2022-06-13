package windymelt

import Types._

import cats._
import cats.implicits._
import com.github.nscala_time.time.Imports._

object Util {
  def ListMonoidIsMonoid[F: Monoid]: Monoid[List[F]] =
    Monoid.instance(
      List.empty[F],
      { case (x, y) =>
        x.zipAll(y, Monoid[F].empty, Monoid[F].empty).map { case (xx, yy) =>
          xx |+| yy
        }
      }
    )
  def splitAndCombine[A, K, F: Monoid](in: List[A])(by: A => K)(
      f: List[A] => List[F]
  ) =
    in.groupBy(by).values.map(f).toList.combineAll(ListMonoidIsMonoid[F])

  private type DatePair = (DateTime, DateTime)
  private val interpolateMonth: List[Action] => DatePair => ByMonth[Action] =
    (xs: ByMonth[Action]) => { case (from: DateTime, until: DateTime) =>
      // 第一段階: xsのうち最大と最小のoccurred_atの間が内挿される
      val zs: List[ByMonth[Action]] = xs coflatMap { ys =>
        val diffMonthsF = (x: Action, y: Action) =>
          x.occurredAt.to(y.occurredAt).toPeriod().getMonths() - 1
        val diffMonths = (ys.get(0), ys.get(1)) mapN diffMonthsF getOrElse (0)
        List(ys(0)) ++ Iterator.continually(ys(0)).take(diffMonths).toList
      }
      // List[List[Action]] になっているので、つぶす
      zs.flatten
    }

  private val extrapolateMonth
      : DatePair => ByMonth[Action] => ByMonth[Option[Action]] = {
    case (from: DateTime, until: DateTime) =>
      (xs: ByMonth[Action]) => {
        // 第二段階: 計測範囲よりも小さい範囲にユーザの操作が納まっていることがあるので、
        // 計測範囲を満たすように外挿する

        // 先頭は埋めるものがないのでNoneで埋める
        val infiniteNone = Iterator.continually(None)
        val headPaddingCount =
          from.to(xs.head.occurredAt).toPeriod().getMonths() - 1
        val headPadding = infiniteNone.take(headPaddingCount).toList

        // 末尾は最後の状態が継続したものとみなす
        val infiniteLast = Iterator.continually(xs.last.some)
        val lastPaddingCount =
          xs.last.occurredAt.to(until).toPeriod().getMonths() - 1
        val lastPadding = infiniteLast.take(lastPaddingCount).toList

        // 外挿する
        headPadding ++ xs.map(_.some) ++ lastPadding
      }
  }

  val interExtrapolateMonth =
    extrapolateMonth <*> interpolateMonth(_: List[Action])

  import Action.actionOrdering
  val inToByMonth: List[Action] => ByMonth[Action] = (in: List[Action]) =>
    in
      .groupBy(a => (a.occurredAt.year, a.occurredAt.month))
      .map { case (k, v) => k -> v.sorted.last } // 月の最後のActionを代表値とみなす
      .values
      .toList
      .sorted
}
