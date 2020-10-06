import scala.annotation.tailrec

case class Transform(val inp: Source, val out: Sink) {
  sealed abstract class Result
  case class Depleted (i: inp.Leftovers, o: out.Iterator ) extends Result
  case class Saturated(i: inp.Iterator , o: out.Leftovers) extends Result

  final def exec(f: inp.Element => out.Element) {
    @tailrec def loop(i: inp.Iterator, o: out.Iterator): Result =
      inp.get(i) match {
        case inp.Done(i) => Depleted(i, o)
        case inp.More(e, i) =>
          out.put(o, f(e)) match {
            case out.Done(o) => Saturated(i, o)
            case out.More(o) => loop(i, o)
          }
      }
    loop _
  }
}
