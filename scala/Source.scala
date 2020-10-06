trait Source {
  type Iterator
  type Element
  type Leftovers

  sealed abstract class Result
  case class More(e: Element, i: Iterator) extends Result
  case class Done(l: Leftovers)            extends Result

  def get(i: Iterator): Result
}

object Source {
  type Of[T] = Source { type Element = T }
}
