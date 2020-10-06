object Chain {
  case class Sources[T](val front: Source.Of[T], val back : Source.Of[T]) extends Source {
    sealed abstract class Iterator

    case class MoreFront
      ( fi: front.Iterator
      , bi: back .Iterator
      ) extends Iterator

    case class MoreBack
      ( fl: front.Leftovers
      , bi: back .Iterator
      ) extends Iterator

    type Element   = T
    type Leftovers = (front.Leftovers, back.Leftovers)

    final def get(i: Iterator) = i match {
      case MoreFront(fi, bi) => front.get(fi) match {
        case front.More(e, fi) => More( e, MoreFront(fi, bi) )
        case front.Done(fl)    => get (    MoreBack (fl, bi) )
      }
      case MoreBack(fl, bi) => back.get(bi) match {
        case back.More(e, bi) => More( e, MoreBack(fl, bi) )
        case back.Done(bl)    => Done(            (fl, bl) )
      }
    }
  }

  case class Sinks[T](val front: Sink.Of[T], val back: Sink.Of[T]) extends Sink {
    sealed abstract class Iterator

    case class MoreFront
      ( fi: front.Iterator
      , bi: back .Iterator
      ) extends Iterator

    case class MoreBack
      ( fl: front.Leftovers
      , bi: back .Iterator
      ) extends Iterator

    type Element   = T
    type Leftovers = (front.Leftovers, back.Leftovers)

    final def put(i: Iterator, e: Element) = i match {
      case MoreFront(fi, bi) => front.put(fi, e) match {
        case front.More(fi) => More( MoreFront(fi, bi) )
        case front.Done(fl) => More( MoreBack (fl, bi) )
      }
      case MoreBack(fl, bi) => back.put(bi, e) match {
        case back.More(bi) => More( MoreBack(fl, bi) )
        case back.Done(bl) => Done(         (fl, bl) )
      }
    }
  }
}
