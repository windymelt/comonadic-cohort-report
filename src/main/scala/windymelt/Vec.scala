package windymelt

sealed trait Vec
case class Start(plan: Plan) extends Vec
case class Change(plan: Plan) extends Vec
case object Exit extends Vec
