functor ZipSources (
  structure F : SOURCE
  structure S : SOURCE ) =
struct
  type iterator = F.iterator * S.iterator
  type element  = F.element  * S.element
  
  datatype leftovers
    = MoreF of F.element * F.iterator * S.leftovers
    | MoreS of F.leftovers * S.element * S.iterator
    | Neither of F.leftovers * S.leftovers
  
  datatype result = More of element * iterator
                  | Done of leftovers
  
  fun get (fi, si) =
    case (F.get fi, S.get si) of
        (F.More (fe, fi), S.More (se, si)) => More ((fe, se), (fi, si))
      | (F.More (fe, fi), S.Done sl      ) => (Done o MoreF) (fe, fi, sl)
      | (F.Done fl      , S.More (se, si)) => (Done o MoreS) (fl, se, si)
      | (F.Done fl      , S.Done sl      ) => (Done o Neither) (fl, sl)
end
