package windymelt

object Types {
  type Id = Int
  type Count = Int
  type ~>[K, V] = Map[K, V]
  type ByMonth[A] = List[A]

  import com.github.nscala_time.time.Imports._
  type TransitionCountWithTiming = (Vec, Vec) ~> ((DateTime, DateTime) ~> Count)

  type CohortPerTransition = Map[Int, Map[DateTime, Count]]
}
