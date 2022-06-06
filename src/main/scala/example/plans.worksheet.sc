// コホートレポートを作りたいんだよね。
// コホートレポートで調べたいこと:
// - いつ入会した人が、いつ退会あるいはプラン変更をしたのか
// 例えば・・・ 6月に入会した人はどんな感じで退会してるんだろう
// Scala + Cats でコホートレポートを実装してみよう。

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports._ // for DateTime

// まずは各種のドメイン知識を実装しよう。
// ユーザが選択できるプランの一覧だ。
sealed trait Plan
case object Premium extends Plan
case object SuperPremium extends Plan

// 次に、ユーザが引き起こすプランの変更についての知識を実装しよう。
sealed trait Vec
case class Start(plan: Plan) extends Vec
case class Change(plan: Plan) extends Vec
case object Exit extends Vec

// ちょっと便利な型エイリアスの類を実装しておく。
type Id = Int
type Count = Int

// ユーザのプラン変更は都度時刻とともに記録されている。これが入力になる。
case class Action(id: Id, user: Id, vec: Vec, occurredAt: DateTime)
type In = Seq[Action]

// まずは、一定期間の出入りを可視化してみよう。つまり、月ごとに特定のプラン変更がどのくらい起こったかだ。
// 簡単のために、プラン変更を決め打ちして、回数だけ記録することにしよう。
type ~>[K, V] = Map[K, V]
type Out = DateTime ~> Seq[Count]

// テストデータはこんな感じでいいか。
// また簡単のために、全部同一ユーザの場合について計算しよう。
val from = DateTime.parse("2022-01-01T00:00:00")
val until = DateTime.parse("2023-02-01T00:00:00")
val testIn0: In = Seq(
  Action(1, 123, Start(SuperPremium), DateTime.parse("2022-06-01T00:00:00")),
  Action(2, 123, Change(Premium), DateTime.parse("2022-07-01T00:00:00")),
  Action(3, 123, Exit, DateTime.parse("2022-08-01T00:00:00")),
  Action(4, 123, Start(Premium), DateTime.parse("2022-09-01T00:00:00")),
  Action(5, 123, Change(SuperPremium), DateTime.parse("2022-10-01T00:00:00")),
  Action(6, 123, Change(Premium), DateTime.parse("2022-11-01T00:00:00")),
  Action(7, 123, Exit, DateTime.parse("2022-12-01T00:00:00"))
)

// 一番小さな部品を作ろう。プラン変更を2つのプランの間の状態遷移だとみなして、
// 状態を2つ受け取り、それが特定のプラン変更であればtrueを返し、そうでなければfalseを返すような関数を考えよう。
val detect0 = (s: Vec) =>
  (x: Vec) =>
    (s, x) match {
      case (Change(Premium), Exit) => true
      case otherwise               => false
    }

// Actionを並べ替えるときにキーとして毎回occurredAtを指示するのは面倒なので、occurredAtで並べることをあらかじめ宣言しておこう。
implicit val actionOrdering: Ordering[Action] = Ordering.by(_.occurredAt)

// 毎月ごとの状態遷移を知りたいので、型エイリアスでわかりやすくしておく。
type ByMonth[A] = List[A]
// 前処理として、Inを毎月のデータに整形する関数を書こう。
// ある月に起こったユーザ操作のうち最後の操作を代表値として、その月にはその操作しかやっていないことにする。
// 何もない月があるとデータが欠損してしまうけれど、これはあとで修正する。
val inToByMonth: In => ByMonth[Action] = (in: In) =>
  in
    .groupBy(a => (a.occurredAt.year, a.occurredAt.month))
    .map { case (k, v) => k -> v.sorted.last }
    .values
    .toList
    .sorted

// 試しにtestIn0に対してこの前処理を実行してみよう:
inToByMonth(testIn0)
// 1つの月に1つの操作しかしていないから、ここでは何も起こらない。

// さて、毎月ユーザが行った操作がリストで得られたから、これを走査して状態遷移を検査し、その月に望みのプラン変更があるかどうかを検出しよう:
def getDesiredMoment0(xs: ByMonth[Action]): Option[Seq[Boolean]] = {
  // ユーザがなにもしていない場合は処理できないので、NonEmptyListを挟む。
  val maybeNel = NonEmptyList.fromList(xs)
  maybeNel map { ys: NonEmptyList[Action] =>
    // 高度な型テクニック。NonEmptyListはComonadになるので、coflatMapを使う。
    // zsには Nel(1,2,3), Nel(2,3), Nel(3)のように1つずつずれたリストが渡ってくる。
    val nested = ys coflatMap { (zs: NonEmptyList[Action]) =>
      // 高度な型テクニックその2。一番最後の要素まで走査すると、zs.get(1)はNoneになってしまう。
      // アプリカティブを用いて、zs.get(1)がNoneならば全体もNoneになるような計算を組み上げる。
      (Some(zs.head), zs.get(1)) mapN { (fst, snd) =>
        detect0(fst.vec)(snd.vec)
      }
    }
    // 最後の要素はつねにNoneになるので、flattenで除外してよい。
    nested.toList.flatten
  }
}

// ここまでの処理を結合すると、特定のプラン変更があった月だけtrueになっている。
getDesiredMoment0(inToByMonth(testIn0))

// 単一のユーザの単一の状態遷移についての、さらに月内の振舞いを単純化した状態のコホートを生成することができた。

// 本当は数字が必要なので、数字を返すようにdetect0を修正する。
val detect = (s: Vec) =>
  (x: Vec) =>
    (s, x) match {
      case (Change(Premium), Exit) => 1
      case otherwise               => 0
    }

// getStartExitMoment0も改良を加えよう。
// 後からdetectを修正できるように、detectを引数として受け取るようにする。
// そして、最終的に足し算ができれば何を返してもよいので、Count(Int)のかわりにF : Monoidとして型を制約する。
def getDesiredMoment[F: Monoid](
    detector: Vec => Vec => F
)(xs: ByMonth[Action]): Option[Seq[F]] = {
  val maybeNel = NonEmptyList.fromList(xs)
  maybeNel map { ys: NonEmptyList[Action] =>
    val nested = ys coflatMap { (zs: NonEmptyList[Action]) =>
      (Some(zs.head), zs.get(1)) mapN { (fst, snd) =>
        detector(fst.vec)(snd.vec)
      }
    }
    nested.toList.flatten
  }
}

// 今度は数字を取り出すことができた。これをユーザごとに行って重ね合わせれば、月ごとの特定のプラン変更を行った人数を算出できる。
getDesiredMoment(detect)(inToByMonth(testIn0))

// これまでは単一のユーザだったが、ユーザの数を増加させる。
val testInA: In = Seq(
  Action(1, 123, Start(SuperPremium), DateTime.parse("2022-06-01T00:00:00")),
  Action(2, 123, Change(Premium), DateTime.parse("2022-07-01T00:00:00")),
  Action(3, 123, Exit, DateTime.parse("2022-08-01T00:00:00")),
  Action(4, 123, Start(Premium), DateTime.parse("2022-09-01T00:00:00")),
  Action(5, 123, Change(SuperPremium), DateTime.parse("2022-10-01T00:00:00")),
  Action(6, 123, Change(Premium), DateTime.parse("2022-11-01T00:00:00")),
  Action(7, 123, Exit, DateTime.parse("2022-12-01T00:00:00"))
)

val testInB: In = Seq(
  Action(8, 456, Start(SuperPremium), DateTime.parse("2022-07-01T00:00:00")),
  Action(9, 456, Change(Premium), DateTime.parse("2022-08-01T00:00:00")),
  // 毎月Actionが起こるわけではない。
  Action(9, 456, Exit, DateTime.parse("2022-12-01T00:00:00"))
)

val testIn = testInA |+| testInB

// とはいっても、実際はユーザごとに計算してあとから結果を結合すればよい。

// testInBには歯脱けの月があるので、これを前処理する関数を定義しよう。
// 空白の月を内挿・外挿する関数を用意する。これは直前の状態が継続しているとみなす。
type DatePair = (DateTime, DateTime)
val interpolateMonth: List[Action] => DatePair => ByMonth[Action] =
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

val extrapolateMonth: DatePair => ByMonth[Action] => ByMonth[Option[Action]] = {
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

// interpolate と extrapolate とを合成する:
val interExtrapolateMonth =
  extrapolateMonth <*> interpolateMonth(_: List[Action])

// testInBを補完してみよう:
interExtrapolateMonth(inToByMonth(testInB))(from, until)

// ところで、補完を行うと ByMonth[Option[Action]] になってしまうので、 getDesiredMoment に渡せない。
// getDesiredMoment が ByMonth[Option] を受け取れるようにする。
def getDesiredMomentOpt[F: Monoid](
    detector: Vec => Vec => F
)(xs: ByMonth[Option[Action]]): Option[Seq[F]] = {
  val maybeNel = NonEmptyList.fromList(xs)
  maybeNel map { ys: NonEmptyList[Option[Action]] =>
    val nested = ys coflatMap { (zs: NonEmptyList[Option[Action]]) =>
      (Some(zs.head), zs.get(1)) mapN { (fst, snd) =>
        // FがMonoidであるおかげで、Noneが渡ってきたときに何を返せばよいかは自明に定まる。
        ((fst, snd) mapN { (f, s) => detector(f.vec)(s.vec) }).orEmpty
      }
    }
    nested.toList.flatten
  }
}

// getDesiredMomentOpt が望み通りのふるまいをするか確認しよう。
getDesiredMomentOpt(detect)(
  interExtrapolateMonth(inToByMonth(testInB))(from, until)
)
// 何も操作のない時期はきちんと0を返せている。

// ユーザごとに計算できるか確認してみよう。
val cohortPerUser0 = testIn groupBy (_.user) map { case (k, v) =>
  k -> getDesiredMomentOpt(detect)(
    interExtrapolateMonth(inToByMonth(v))(from, until)
  )
}
// ユーザ単位で特定の操作を行った月を出力できている。

// あとは、ユーザごとの結果を結合するだけだ。getDesiredMomentOpt が Option[Seq[F : Monoid]] を返すおかげで、 combineAll を呼べば勝手に結果が結合される。
// ただし、一度transposeで転置しなければならないのが面倒だが。
val cohort0 = cohortPerUser0.values.toList.flatten.transpose
  .traverse(xs => List(xs.combineAll))
  .flatten
// お見事。

// 複数のユーザの単一の状態遷移についての、さらに月内の振舞いを単純化した状態のコホートを生成することができた。
// 今度は、複数の状態遷移を扱えるようにして一覧できるようにしよう。
// これはdetectを拡張すればよい。
type TransitionCount = (Vec, Vec) ~> Count
val detectMulti: Vec => Vec => TransitionCount = (s: Vec) =>
  (x: Vec) => Map[(Vec, Vec), Count](((s, x), 1))

// ちなみにTransitionCountはMonoidになる:
val tc = Map(((Start(Premium), Exit), 1))
tc |+| tc |+| tc

// detectMultiをgetStartExitMomentOptに渡してみよう:
getDesiredMomentOpt(detectMulti)(
  interExtrapolateMonth(inToByMonth(testInB))(from, until)
)
// TransitionCount が Monoidであるおかげで、他のどこにも手を加えることなく勝手に動いた。すげ〜

// さて、補完の影響で同じ状態間の遷移も記録されてしまってレポートの邪魔だ。同じ状態遷移は禁止する。
val detectMultiWithNoSameVec: Vec => Vec => TransitionCount = (s: Vec) =>
  (x: Vec) =>
    (s, x) match {
      case (s, x) if s == x => Map.empty // Exclude same transition
      case (s, x)           => Map[(Vec, Vec), Count](((s, x), 1))
    }

// これですっきりした。
getDesiredMomentOpt(detectMultiWithNoSameVec)(
  interExtrapolateMonth(inToByMonth(testInB))(from, until)
)
// 同じ状態間の遷移は見えなくなった。

// さっそくtestInで試してみよう:
val multipleCohortPerUser = testIn groupBy (_.user) map { case (k, v) =>
  k -> getDesiredMomentOpt(detectMultiWithNoSameVec)(
    interExtrapolateMonth(inToByMonth(v))(from, until)
  )
}
// 個々のユーザの状態遷移が記録された。

// あとはいつもどおりvaluesしてtoListしてflattenしてtransposeしてtraverseしてflattenしよう。
multipleCohortPerUser.values.toList.flatten.transpose
  .traverse(xs => List(xs.combineAll))
  .flatten
// なんてこった!! 複数のユーザの状態遷移を集計できてしまった。

// 複数のユーザの複数の状態遷移をまとめることができた。
// そういえば、コホートレポートなのでnヶ月後に退会したといったデータを取り出したい。
// どのくらい後に退会したというデータはユーザ単位でしか追跡できないので、まずユーザの状態遷移を取り出して、
// 月ごとに・数ヶ月後(計測期間の終わり)にどうなったか、という形式にマッピングする必要がある。
// 複数のユーザの状態遷移を集計してしまった状態だと、データが潰れてしまう。
// ちょっと、TransitionCountの型に注目してみよう: type TransitionCount = (Vec, Vec) ~> Count
// この状態だとギリギリマッピングできない。時刻の情報が消失してしまったからだ。
// (DateTime, Vec, DateTime, Vec) ~> Count ならどうだろう？ これなら情報は消失しない(面倒そうだけど)。
// Countの代わりになんかMonoidalな値を考えるのはどうだろう？
// (Vec, Vec) ~> ((DateTime, DateTime) ~> Count) は、たぶんMonoidになるはずだ。
// なぜかというと、Monoidalなvalueを持つMapはMonoidになるからだ。これは重畳できる。
type TransitionCountWithTiming = (Vec, Vec) ~> ((DateTime, DateTime) ~> Count)
val AssumeTCWTIsMonoid = implicitly[Monoid[TransitionCountWithTiming]]
// コンパイルが通るので、やっぱりモノイドだ。
// でもdetectMultiWithNoSameVecのTransitionCountWithTiming版、作れるかなあ・・・
// occurredAtの情報が欲しいから、VecではなくActionが必要だ。
val detectMultiWithNoSameVec2: Action => Action => TransitionCountWithTiming =
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
// 気絶しそうだ。

// getDesiredMomentOptはVecを渡してくるので、Actionを渡すように修正する:
def getDesiredMomentOpt2[F: Monoid](
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

// とりあえず動くか確かめてみよう:
val cohortWithTiming = testIn groupBy (_.user) map { case (k, v) =>
  k -> getDesiredMomentOpt2(detectMultiWithNoSameVec2)(
    interExtrapolateMonth(inToByMonth(v))(from, until)
  )
}
val combinedCohortWithTiming = cohortWithTiming.values.toList.flatten.transpose
  .traverse(xs => List(xs.combineAll))
  .flatten

// うまく動いてそうだ。この構造の利点は、TransitionCountへと潰せることだ:
val shrinkToTransitionCount =
  combinedCohortWithTiming.map(_.map { case (k, v) =>
    k -> v.values.toList.combineAll
  })

// TransitionCountWithTiming を pretty printする関数を考えてみよう。
// 典型的には、ある状態から別の状態への遷移に注目して、時系列でどのように変化するかをprintする。
val filteredCohortWithTiming = combinedCohortWithTiming.map(_.collect {
  case ((Change(Premium), Exit), v) => v
})
// 0ヶ月、1ヶ月、2ヶ月と差分をもとに表示したいので、さらに変形する:
val filteredCohortWithTimingDiff =
  filteredCohortWithTiming.flatten.combineAll.map { case (((f, u), v)) =>
    f.to(u).toPeriod().getMonths() -> (f -> v) // FIX: つぶしすぎている
  }
(0 to 12) foreach { diff =>
  filteredCohortWithTimingDiff.get(diff)
}
// WIP