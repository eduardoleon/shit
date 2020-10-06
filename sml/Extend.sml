functor Extend (
  structure I : SOURCE
  structure E : SINK   where type element = I.element
  structure L : SINK   where type element = I.leftovers ) =
struct
  type iterator = E.iterator * L.iterator
  type element  = I.iterator
  
  datatype leftovers
    = MoreE of E.iterator  * L.leftovers
    | MoreL of E.leftovers * L.iterator  * I.iterator
  
  datatype result = More of iterator
                  | Done of leftovers
  
  local
    fun iter ii =
      case I.get ii of
          I.More ieii => elem ieii
        | I.Done il   => left il
    
    and elem (ee, ii) (ei, li) =
      case E.put (ei, ee) of
          E.More ei => iter ii        (ei, li)
        | E.Done el => (Done o MoreL) (el, li, ii)
    
    and left le (ei, li) =
      case L.put (li, le) of
          L.More li => More           (ei, li)
        | L.Done ll => (Done o MoreE) (ei, ll)
  in
    fun put (i, e) = iter e i
  end
end
