structure Either = struct
  datatype ('a, 'b) either = Left  of 'a
                           | Right of 'b
  
  fun either f g =
    let
      fun pick (Left  x) = f x
        | pick (Right y) = g y
    in
      pick
    end
end
