object Zip {
  case class Sources(val first: Source, val second: Source) extends Source {
    type Iterator = (first.Iterator, second.Iterator)
    type Element  = (first.Element , second.Element )

    sealed abstract class Leftovers

    case class MoreFirst
      ( fe: first .Element
      , fi: first .Iterator
      , sl: second.Leftovers
      ) extends Leftovers

    case class MoreSecond
      ( fl: first .Leftovers
      , se: second.Element
      , si: second.Iterator
      ) extends Leftovers

    case class Neither
      ( fl: first .Leftovers
      , sl: second.Leftovers
      ) extends Leftovers

    final def get(i: Iterator) =
      (first.get(i._1), second.get(i._2)) match {
	case (first.More(fe, fi), second.More(se, si)) =>
	  More( (fe, se), (fi, si) )
	case (first.More(fe, fi), second.Done(sl)) =>
	  Done( MoreFirst (fe, fi, sl) )
	case (first.Done(fl), second.More(se, si)) =>
	  Done( MoreSecond (fl, se, si) )
	case (first.Done(fl), second.Done(sl)) =>
	  Done( Neither (fl, sl) )
      }
  }

  case class Sinks(val first: Sink, val second: Sink) extends Sink {
    type Iterator = (first.Iterator, second.Iterator)
    type Element  = (first.Element , second.Element )

    sealed abstract class Leftovers

    case class MoreFirst
      ( fi: first .Iterator
      , sl: second.Leftovers
      ) extends Leftovers

    case class MoreSecond
      ( fl: first .Leftovers
      , si: second.Iterator
      ) extends Leftovers

    case class Neither
      ( fl: first .Leftovers
      , sl: second.Leftovers
      ) extends Leftovers

    final def put(i: Iterator, e: Element) =
      (first.put(i._1, e._1), second.put(i._2, e._2)) match {
	case (first.More(fi), second.More(si)) => More(		  (fi, si) )
	case (first.More(fi), second.Done(sl)) => Done( MoreFirst (fi, sl) )
	case (first.Done(fl), second.More(si)) => Done( MoreSecond(fl, si) )
	case (first.Done(fl), second.Done(sl)) => Done( Neither	  (fl, sl) )
      }
  }
}
