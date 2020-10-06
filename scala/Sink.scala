trait Sink {
  type Iterator
  type Element
  type Leftovers

  sealed abstract class Result
  case class More(i: Iterator)  extends Result
  case class Done(l: Leftovers) extends Result

  def put(i: Iterator, e: Element): Result
}

object Sink {
  type Of[T] = Sink { type Element = T }
}
