package windymelt

import Types._
import com.github.nscala_time.time.Imports._

case class Action(id: Id, user: Id, vec: Vec, occurredAt: DateTime)
object Action {
  implicit val actionOrdering: Ordering[Action] = Ordering.by(_.occurredAt)
}
