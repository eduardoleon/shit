case class Filter[A,B](val input: Source.Of[Either[A,B]]) {
  case class Lefts(val output: Sink.Of[B]) extends Source {
    type Iterator = (input.Iterator, output.Iterator)
    type Element  = A

    sealed abstract class Leftovers

    case class MoreInput
      ( ii: input .Iterator
      , ol: output.Leftovers
      ) extends Leftovers

    case class MoreOutput
      ( il: input .Leftovers
      , oi: output.Iterator
      ) extends Leftovers

    final def get(i: Iterator) = input.get(i._1) match {
      case input.Done(il) => Done( MoreOutput(il, i._2) )
      case input.More(ie, ii) => ie match {
        case Left(e) => More( e, (ii, i._2) )
        case Right(oe) => output.put(i._2, oe) match {
          case output.Done(ol) => Done( MoreInput(ii, ol) )
          case output.More(oi) => get (          (ii, oi) )
        }
      }
    }
  }

  case class Rights(val output: Sink.Of[A]) extends Source {
    type Iterator = (input.Iterator, output.Iterator)
    type Element  = B

    sealed abstract class Leftovers

    case class MoreInput
      ( ii: input .Iterator
      , ol: output.Leftovers
      ) extends Leftovers

    case class MoreOutput
      ( il: input .Leftovers
      , oi: output.Iterator
      ) extends Leftovers

    final def get(i: Iterator) = input.get(i._1) match {
      case input.Done(il) => Done( MoreOutput(il, i._2) )
      case input.More(ie, ii) => ie match {
        case Right(e) => More( e, (ii, i._2) )
        case Left(oe) => output.put(i._2, oe) match {
          case output.Done(ol) => Done( MoreInput(ii, ol) )
          case output.More(oi) => get (          (ii, oi) )
        }
      }
    }
  }
}
