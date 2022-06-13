import cats._
import cats.implicits._
import cats.data.ZipList

val xs = List(1, 2, 3)
val ys = List(4, 5, 6)
val zs = List(7, 8, 9)

val Seq(xsZ, ysZ, zsZ) = Seq(xs, ys, zs).map(ZipList.apply)

val f = (x: Int) => (y: Int) => (z: Int) => x + y + z

xs zip ys

// List[F : Monoid] は Monoidになる:
//xs |+| ys

// しかし別の結合方法を考えることもできる。
// それぞれをzipしてmapするような方法がある。
xs.zip(ys).map(pair => pair._1 |+| pair._2)

implicit def ListMonoidIsMonoid[F: Monoid]: Monoid[List[F]] =
  Monoid.instance(
    List.empty[F],
    { case (x, y) =>
      x.zipAll(y, Monoid[F].empty, Monoid[F].empty).map { case (xx, yy) =>
        xx |+| yy
      }
    }
  )

ListMonoidIsMonoid[Int].combine(xs, ys)

// monoid law
val e = List[Int]()
ListMonoidIsMonoid[Int].combine(xs, e) == xs
ListMonoidIsMonoid[Int].combine(e, xs) == xs
ListMonoidIsMonoid[Int].combine(e, e) == e
val leftwards = List(xs, ys, zs).foldLeft(ListMonoidIsMonoid[Int].empty) {
  case (x, y) => ListMonoidIsMonoid[Int].combine(x, y)
}
val rightwards = List(xs, ys, zs).foldRight(ListMonoidIsMonoid[Int].empty) {
  case (x, y) => ListMonoidIsMonoid[Int].combine(x, y)
}
leftwards == rightwards

ListMonoidIsMonoid[Int].combineAll(Seq(xs, ys, zs))
