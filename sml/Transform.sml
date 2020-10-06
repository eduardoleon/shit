functor Transform (
  structure I : SOURCE
  structure O : SINK ) =
struct
  datatype result = MoreI of I.iterator  * O.leftovers
                  | MoreO of I.leftovers * O.iterator
  
  fun exec f =
    let
      fun loop (ii, oi) =
        case I.get ii of
            I.Done il => MoreO (il, oi)
          | I.Cont (e, ii) =>
            case O.put (oi, f e) of
                O.Done ol => MoreI (ii, ol)
              | O.Cont oi => loop (ii, oi)
    in
      loop
    end
end
