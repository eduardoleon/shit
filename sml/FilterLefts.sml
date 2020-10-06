functor FilterLefts
  ( type element
    structure O : SINK
    structure I : SOURCE
    where type element = (element, O.element) Either.either ) =
struct
  type iterator = I.iterator * O.iterator
  type element  = element
  
  datatype leftovers = MoreI of I.iterator  * O.leftovers
                     | MoreO of I.leftovers * O.iterator
  
  datatype result = More of element * iterator
                  | Done of leftovers
  
  local
    fun input (ii, oi) =
      case I.get ii of
          I.More (ie, ii) => either ie      (ii, oi)
        | I.Done il       => (Done o MoreO) (il, oi)
    
    and either (Either.Right oe) = output oe
      | either (Either.Left  e ) = fn iter => More (e, iter)
    
    and output oe (ii, oi) =
      case O.put (oi, oe) of
          O.More oi => input          (ii, oi)
        | O.Done ol => (Done o MoreI) (ii, ol)
  in
    val get = input
  end
end
