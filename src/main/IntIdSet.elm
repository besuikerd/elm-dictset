module IntIdSet exposing (..)

import IdSet exposing (..)

{-| IdGenerator for intgers.
-}
type alias IntIdSet item = IdSet Int Int item

{-| IdGenerator for integers that simply increments the seed by 1.
-}
intGenerator : IdGenerator Int Int
intGenerator i = (i, i + 1)

empty: IntIdSet item
empty = IdSet.empty intGenerator 0
