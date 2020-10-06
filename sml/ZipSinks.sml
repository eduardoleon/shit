functor ZipSinks (
  structure F : SINK
  structure S : SINK ) =
struct
  type iterator = F.iterator * S.iterator
  type element  = F.element  * S.element
  
  datatype leftovers
    = MoreF of F.iterator * S.leftovers
    | MoreS of F.leftovers * S.iterator
    | Neither of F.leftovers * S.leftovers
  
  datatype result = More of iterator
                  | Done of leftovers
  
  fun put ((fi, si), (fe, se)) =
    case (F.put (fi, fe), S.put (si, se)) of
        (F.More fi, S.More si) => More             (fi, si)
      | (F.More fi, S.Done sl) => (Done o MoreF)   (fi, sl)
      | (F.Done fl, S.More si) => (Done o MoreS)   (fl, si)
      | (F.Done fl, S.Done sl) => (Done o Neither) (fl, sl)
end
