module IntIdSet exposing (..)

{-| Specilization functions for IdSets containing integers as their id.

@docs IntIdSet
@docs intGenerator
@docs empty
-}

import IdSet exposing (..)

{-| IdGenerator for intgers.
-}
type alias IntIdSet item = IdSet Int Int item

{-| IdGenerator for integers that simply increments the seed by 1.
-}
intGenerator : IdGenerator Int Int
intGenerator i = (i, i + 1)

{-| Creates an empty set with intGenerator as its IdGenerator and initial seed 0
-}
empty: IntIdSet item
empty = IdSet.empty intGenerator 0
