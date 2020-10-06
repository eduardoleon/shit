signature SOURCE = sig
  type iterator
  type element
  type leftovers
  
  datatype result = More of element * iterator
                  | Done of leftovers
  
  val get : iterator -> result
end
