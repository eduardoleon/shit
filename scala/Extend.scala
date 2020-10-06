import scala.annotation.tailrec

case class Extend[E,L](val elem: Sink.Of[E], val left: Sink.Of[L]) {
  type Chunk = Source {
    type Element   = E
    type Leftovers = L
  }

  case class Apply(val iter: Chunk) extends Sink {
    type Iterator = (elem.Iterator, left.Iterator)
    type Element  =  iter.Iterator

    sealed abstract class Leftovers

    case class MoreElem
      ( ei: elem.Iterator
      , ll: left.Leftovers
      ) extends Leftovers

    case class MoreLeft
      ( el: elem.Leftovers
      , li: left.Iterator
      , ii: iter.Iterator
      ) extends Leftovers

    @tailrec final def put(i: Iterator, e: Element) = iter.get(e) match {
      case iter.More(ie, ii) => elem.put(i._1, ie) match {
        case elem.More(ei) => put (         (ei, i._2), ii )
        case elem.Done(el) => Done( MoreLeft(el, i._2 , ii) )
      }
      case iter.Done(il) => left.put(i._2, il) match {
        case left.More(li) => More(         (i._1, li) )
        case left.Done(ll) => Done( MoreElem(i._1, ll) )
      }
    }
  }
}
