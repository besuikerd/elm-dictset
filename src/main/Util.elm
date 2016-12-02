module Util exposing
  ( expand1
  )

expand1 : (b -> c) -> (a -> b -> c)
expand1 fn =
  let
    expandedFn a b = fn b
  in
    expandedFn
