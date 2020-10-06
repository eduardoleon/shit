import scala.annotation.tailrec

case class Flatten[I,L](val iter: Source.Of[I], val left: Sink.Of[L]) {
  type Chunk = Source {
    type Iterator  = I
    type Leftovers = L
  }

  sealed abstract class Leftovers

  case class MoreLeft
    ( li: left.Iterator
    , il: iter.Leftovers
    ) extends Leftovers

  case class MoreIter
    ( ll: left.Leftovers
    , ii: iter.Iterator
    ) extends Leftovers

  case class Apply(val elem: Chunk) extends Source {
    type Iterator  = (elem.Iterator, left.Iterator, iter.Iterator)
    type Element   =  elem.Element
    type Leftovers = Flatten.this.Leftovers

    @tailrec final def get(i: Iterator) = elem.get(i._1) match {
      case elem.More(ee, ei) => More( ee, (ei, i._2, i._3) )
      case elem.Done(el) => left.put(i._2, el) match {
        case left.Done(ll) => Done( MoreIter(ll, i._3) )
        case left.More(li) => iter.get(i._3) match {
          case iter.Done(il) => Done( MoreLeft(li, il) )
          case iter.More(ie, ii) => get( (ie, li, ii) )
        }
      }
    }
  }
}
