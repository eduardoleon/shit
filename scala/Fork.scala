case class Fork(val left: Sink, val right: Sink) extends Sink {
  type Iterator =       (left.Iterator, right.Iterator)
  type Element  = Either[left.Element , right.Element ]

  sealed abstract class Leftovers

  case class MoreLeft
    ( li: left .Iterator
    , rl: right.Leftovers
    ) extends Leftovers

  case class MoreRight
    ( ll: left .Leftovers
    , ri: right.Iterator
    ) extends Leftovers

  final def put(i: Iterator, e: Element) = e match {
    case Left(e) => left.put(i._1, e) match {
      case left.More(li) => More(          (li, i._2) )
      case left.Done(ll) => Done( MoreRight(ll, i._2) )
    }
    case Right(e) => right.put(i._2, e) match {
      case right.More(ri) => More(         (i._1, ri) )
      case right.Done(rl) => Done( MoreLeft(i._1, rl) )
    }
  }
}
