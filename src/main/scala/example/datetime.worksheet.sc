import com.github.nscala_time.time.Imports._

val d1 = new DateTime("2022-01-01T00:00:00+09:00")
val d2 = new DateTime("2023-01-01T00:00:00+09:00")

d1.to(d2).toPeriod().
