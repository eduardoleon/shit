functor ChainSinks (
  structure F : SINK
  structure B : SINK where type element = F.element ) =
struct
  datatype iterator = MoreF of F.iterator  * B.iterator
                    | MoreB of F.leftovers * B.iterator
  
  type element   = F.element
  type leftovers = F.leftovers * B.leftovers
  
  datatype result = More of iterator
                  | Done of leftovers
  
  local
    fun front (fi, bi) e =
      case F.put (fi, e) of
          F.More fi => (More o MoreF) (fi, bi)
        | F.Done fl => (More o MoreB) (fl, bi)
    
    fun back (fl, bi) e =
      case B.put (bi, e) of
          B.More bi => (More o MoreB) (fl, bi)
        | B.Done bl => Done           (fl, bl)
  in
    fun put (MoreF iter, e) = front iter e
      | put (MoreB iter, e) = back  iter e
  end
end
