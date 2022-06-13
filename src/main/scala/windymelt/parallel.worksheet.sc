// モノイドからなるリストのリストを垂直に結合したい。

import cats._
import cats.implicits._

// こいつらをぜんぶくっつけたい
val xs = (1 to 9).toList
val ys = (1 to 9).toList
val zs = (1 to 9).toList
val lis = List(xs, ys, zs)

// 素朴にcombineAllするとリストは単純に結合されてしまう。
lis.combineAll

// ここで便利データ構造であるZipListを使う。
import cats.data.ZipList
// ZipListはApplyなので、apを使うことができる。
// apは、複数のZipListから1要素ずつ取り出して関数に適用するという振舞いをとる。
val f = (x: Int) => (y: Int) => x + y
val zlisAp =
  ZipList(List(f, f, f)) <*> ZipList(xs) <*> ZipList(ys)
zlisAp.value

// ちなみに通常のListにapするとcartesian productになり、各要素の全ての組を取り出そうとする
val g = (x: Int) => (y: Int) => x * y
val kuku = List(g) <*> xs <*> ys
kuku.grouped(9).toList

// しかし、Apply単体ではZipListのリストを一気にreduceして畳み込むことができない。
// そこで、Applyにある便利機能を使う。
// Apply.semigroupは、FがApplyで、AがSemigroupであるとき、Semigroup[F[A]]のインスタンスを導出できる。(なんでかって？わからない)
// 今回はZipListがApplyで、IntはSemigroupなので、ZipList[Int]がSemigroupになった。
implicit val ZipListIsSemigroup: Semigroup[ZipList[Int]] = Apply.semigroup

// xs, ys, zsをZipListにして・・・
val zlis @ Seq(xsz, ysz, zsz) = lis map ZipList.apply

// xsz, ysz, zszはSemigroupになったので、直接combineできるようになった。
(xsz |+| ysz).value

// このまま一気にcobmineAllしたいが、combineAllするにはMonoidである必要がある。
// 今回はSemigroupのインスタンスしか無いので、より制約の緩いかわりにOptionを返す
// combineAllOptionを使う。
val result = zlis.combineAllOption
result.map(_.value)

// 一度xs, ys, zsをZipListに変換し、Apply[ZipList[Int]]から導出したSemigroup[ZipList[Int]]を用いてcombineAllOptionを成功させた。
// その後ZipListをListに戻し、結果の値を得た。

// ちなみに、ListとZipListはNonEmptyParallelの関係にある。
val instance = implicitly[NonEmptyParallel[List]]
lis.map(instance.parallel(_))

// NonEmptyParallelに定義されているメソッドを呼ぶために、lisをNonEmptyListにしておくと、色々と便利な事が起こる:
import cats.data.NonEmptyList
NonEmptyList.fromListUnsafe(lis).parReduceMapA(NonEmptyList.fromListUnsafe(_))

// あるいは最初から全てNonEmptyListにしておく:
val nelxs = NonEmptyList.fromList((1 to 9).toList).get
val nelys = NonEmptyList.fromList((1 to 9).toList).get
val nelzs = NonEmptyList.fromList((1 to 9).toList).get
val nel = NonEmptyList(nelxs, List(nelys, nelzs))

nel.parReduceMapA(identity)
// parReduceMapは、いったんNonEmptyList中の要素をparallel変換してZipListにしたあと、NonEmptyList[ZipList]をmapし、それをSemigroupを使ってreduceする。mapしなくてもよいならidentityを渡せばよさそうだ。
nel.parReduceMapA(_.map(_.toString))
nel.parReduceMapA(_.reverse)

// ただし、Nelを挟む面倒さと比べると、lis map ZipList.applyするのとあまり変わらないと思う。