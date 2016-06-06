module Util exposing
  ( expand1
  )

expand1 : (b -> c) -> (a -> b -> c)
expand1 fn =
  let
    fn' a b = fn b
  in
    fn'
