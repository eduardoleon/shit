functor ForkSinks (
  structure L : SINK
  structure R : SINK ) =
struct
  type iterator =  L.iterator * R.iterator
  type element  = (L.element  , R.element ) Either.either
  
  datatype leftovers = MoreL of L.iterator  * R.leftovers
                     | MoreR of L.leftovers * R.iterator
  
  datatype result = More of iterator
                  | Done of leftovers
  
  local
    fun left (li, ri) le =
      case L.put (li, le) of
          L.More li => More           (li, ri)
        | L.Done ll => (Done o MoreR) (ll, ri)
    
    fun right (li, ri) re =
      case R.put (ri, re) of
          R.More ri => More           (li, ri)
        | R.Done rl => (Done o MoreL) (li, rl)
  in
    fun put (iter, Either.Left  le) = left  iter le
      | put (iter, Either.Right re) = right iter re
  end
end
