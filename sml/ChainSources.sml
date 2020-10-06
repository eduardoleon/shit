functor ChainSources (
  structure F : SOURCE
  structure B : SOURCE where type element = F.element ) =
struct
  datatype iterator = MoreF of F.iterator  * B.iterator
                    | MoreB of F.leftovers * B.iterator
  
  type element   = F.element
  type leftovers = F.leftovers * B.leftovers
  
  datatype result = More of element * iterator
                  | Done of leftovers
  
  local
    fun back (fl, bi) =
      case B.get bi of
          B.More (e, bi) => More (e, MoreB (fl, bi))
        | B.Done bl      => Done           (fl, bl)
    
    fun front (fi, bi) =
      case F.get fi of
          F.More (e, fi) => More (e, MoreF (fi, bi))
        | F.Done fl      => back           (fl, bi)
  in
    fun get (MoreF iter) = front iter
      | get (MoreB iter) = back  iter
  end
end
