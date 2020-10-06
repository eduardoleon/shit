signature SINK = sig
  type iterator
  type element
  type leftovers
  
  datatype result = More of iterator
                  | Done of leftovers
  
  val put : iterator * element -> result
end
