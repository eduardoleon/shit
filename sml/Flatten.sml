functor Flatten (
  structure E : SOURCE
  structure I : SOURCE where type element = E.iterator
  structure L : SINK   where type element = E.leftovers ) =
struct
  type iterator = E.iterator * L.iterator * I.iterator
  type element  = E.element
  
  datatype leftovers
    = MoreL of L.iterator  * I.leftovers
    | MoreI of L.leftovers * I.iterator
  
  datatype result = More of element * iterator
                  | Done of leftovers
  
  local
    fun elem (ei, li, ii) =
      case E.get ei of
          E.More (ee, ei) => More (ee, (ei, li, ii))
        | E.Done el       => left      (el, li, ii)
    
    and left (le, li, ii) =
      case L.put (li, le) of
          L.More li => iter           (li, ii)
        | L.Done ll => (Done o MoreI) (ll, ii)
    
    and iter (li, ii) =
      case I.get ii of
          I.More (ie, ii) => elem       (ie, li, ii)
        | I.Done il       => (Done o MoreL) (li, il)
  in
    val get = elem
  end
end
