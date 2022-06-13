package windymelt

sealed trait Plan
case object Premium extends Plan
case object SuperPremium extends Plan
