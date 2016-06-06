module DictSet exposing
  ( DictSet
  , Ord
  , empty
  , singleton
  , insert
  , update
  , remove
  , isEmpty
  , member
  , get
  , size
  , keys
  , values
  , toList
  , fromList
  , map
  , foldl
  , foldr
  , filter
  , partition
  , union
  , intersect
  , diff
  , merge
  , ord
  , dict
  )

{-|  A Set that contains ordinal values, backed by a Dict. The ordinal value of
items in a DictSet are specified by the ord method that is given whenever a
DictSet is constructed.

# Dictionaries

@docs DictSet
@docs Ord

# Build

@docs empty
@docs singleton
@docs insert
@docs update
@docs remove

# Query

@docs isEmpty
@docs member
@docs get
@docs size

# Lists

@docs keys
@docs values
@docs toList
@docs fromList

# Transform

@docs map
@docs foldl
@docs foldr
@docs filter
@docs partition

# Combine

@docs union
@docs intersect
@docs diff
@docs merge

# Helpers

@docs ord
@docs dict

-}

import Dict exposing (Dict)
import Util exposing (..)


-- DICTIONARIES

{-| Ordinal function to define the order of DictSet entries.
-}
type alias Ord k v = v -> k

{-| A set of ordered values ordered by an ordinal function
-}
type DictSet k v = DictSet (Ord k v) (Dict k v)

-- BUILD

{-| Creates an empty DictSet using the specified ordinal function.
-}
empty : Ord k v -> DictSet k v
empty ord = DictSet ord Dict.empty

{-| Creates a singleton set - using the specified ordinal function.
-}
singleton : Ord comparable v -> v -> DictSet comparable v
singleton ord v = DictSet
  ord
  (Dict.singleton (ord v) v)


{-| Inserts an item into the set.
-}
insert : v -> DictSet comparable v -> DictSet comparable v
insert v dictSet =
  mapDict
    (\ord -> Dict.insert (ord v) v)
    dictSet

{-| Replaces an item identified with the same ordinal value to the given new value
-}
update
  :  v
  -> (Maybe v -> Maybe v)
  -> DictSet comparable v
  -> DictSet comparable v
update v fn dictSet =
  mapDict
    (\ord -> Dict.update (ord v) fn)
    dictSet

{-| Removes an item from the set if it exists.
-}
remove : v -> DictSet comparable v -> DictSet comparable v
remove v dictSet =
  mapDict
    (\ord -> Dict.remove (ord v))
    dictSet

-- QUERY

{-| Determine if the set is empty.
-}
isEmpty : DictSet k v -> Bool
isEmpty = wrap Dict.isEmpty

{-| Determine if an item is a member of the set.
-}
member : v -> DictSet comparable v -> Bool
member v dictSet =
  let
    (ord, dict) = unwrap dictSet
  in
    Dict.member (ord v) dict

{-| Get the value from the set if it exists. Does not guarantee that the items
are the same, it only retrieves an item with a matching ordinal value if it
exists.
-}
get : comparable -> DictSet comparable v -> Maybe v
get = wrap << Dict.get

{-| Determines the number of items in a set
-}
size : DictSet k v -> Int
size = wrap Dict.size

-- LISTS

{-| Get a list of keys (the ordinal values) of items in the set, ordered from
lowest to highest.
-}
keys : DictSet comparable v -> List comparable
keys = wrap Dict.keys

{-| Get a list of values of items in the set, ordered from lowest to highest.
-}
values : DictSet comparable v -> List v
values = wrap Dict.values

{-| Convert a set into an association list of key-value pairs, sorted by their
ordinal values.
-}
toList : DictSet comparable v -> List (comparable, v)
toList = wrap Dict.toList

{-| Convert a list of items to a `DictSet` with the given ordering.
-}
fromList : Ord comparable v -> List v -> DictSet comparable v
fromList ord vs =
  DictSet
    ord
    (Dict.fromList (List.map (\v -> (ord v, v)) vs))

-- TRANSFORM

{-| Apply a function to all values in the set.
-}
map : (v -> v) -> DictSet comparable v -> DictSet comparable v
map fn dictSet = foldl (\acc cur -> insert (fn cur) acc) (empty (ord dictSet)) dictSet

{-| Fold over all values in the set, in order from lowest key to highest key.
-}
foldl : (b -> v -> b) -> b -> DictSet comparable v -> b
foldl fn init dictSet = Dict.foldl (expand1 <| flip fn) init (dict dictSet)

{-| Fold over all values in the set, in order from highest key to lowest key.
-}
foldr : (v -> b -> b) -> b -> DictSet comparable v -> b
foldr fn init dictSet = (Dict.foldr (expand1 fn) init (dict dictSet))

{-| Filter elements from the set that do not satisfy the predicate.
-}
filter : (v -> Bool) -> DictSet comparable v -> DictSet comparable v
filter pred dictSet =
  mapDict
    (\ord -> Dict.filter (expand1 pred))
    dictSet

{-| Partition a set according to a predicate. The first set contains all
items which satisfy the predicate and the second contains the rest.
-}
partition
  :  (v -> Bool)
  -> DictSet comparable v
  -> (DictSet comparable v, DictSet comparable v)
partition pred dictSet =
  let
    (ord, dict) = unwrap dictSet
    (dictLeft, dictRight) = Dict.partition (expand1 pred) dict
  in
    (DictSet ord dictLeft, DictSet ord dictRight)

-- COMBINE

{-| Combine two sets. If there is a collision, preference is given to the first
set.
-}
union
  :  DictSet comparable v
  -> DictSet comparable v
  -> DictSet comparable v
union = combine Dict.union

{-| Creates a set that contains items that are contained in both sets.
-}
intersect
  :  DictSet comparable v
  -> DictSet comparable v
  -> DictSet comparable v
intersect = combine Dict.intersect

{-| Creates a set that contains all items that are contained in the first set,
but not in the second set.
-}
diff
  :  DictSet comparable v
  -> DictSet comparable v
  -> DictSet comparable v
diff = combine Dict.diff

{-| The most general way of combining two sets. You provide three
accumulators for when a given key appears:

  1. Only in the left set.
  2. In both sets.
  3. Only in the right set.

You then traverse all the items from lowest ordinal value to highest ordinal
value, building up whatever you want.
-}
merge
  :  (a -> result -> result)
  -> (a -> a -> result -> result)
  -> (a -> result -> result)
  -> DictSet comparable a
  -> DictSet comparable a
  -> result
  -> result
merge leftStep bothStep rightStep leftDict rightDict init =
  (Dict.merge
    (expand1 leftStep)
    (expand1 bothStep)
    (expand1 rightStep)
    (dict leftDict)
    (dict rightDict)
    init
  )

-- HELPERS

{-| Obtains the function the set uses to calculate ordinal values for its items
-}
ord : DictSet k v -> Ord k v
ord dictSet = case dictSet of
  DictSet ord _ ->
    ord

{-| Converts a `Dictset` to a `Dict`
-}
dict : DictSet k v -> Dict k v
dict dictSet = case dictSet of
  DictSet _ dict ->
    dict

-- IMPLEMENTATION DETAILS


unwrap : DictSet k v -> (Ord k v, Dict k v)
unwrap dictSet = case dictSet of
  DictSet ord dict ->
    (ord, dict)

mapDict : (Ord k v -> (Dict k v -> Dict k v)) -> DictSet k v -> DictSet k v
mapDict fn dictSet = case dictSet of
  DictSet ord dict ->
    DictSet ord (fn ord <| dict)


wrap : (Dict k v -> b) -> DictSet k v -> b
wrap = (>>) dict


combine
  :  (Dict comparable v -> Dict comparable v -> Dict comparable v)
  -> DictSet comparable v
  -> DictSet comparable v
  -> DictSet comparable v
combine fn ds1 ds2 =
  let
    (ord, d1) = unwrap ds1
    d2 = dict ds2
  in
  DictSet
    ord
    (fn d1 d2)
