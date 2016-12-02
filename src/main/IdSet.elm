module IdSet exposing
  ( IdSet
  , IdGenerator
  , empty
  , singleton
  , insert
  , insertWithId
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
  , mapWithId
  , foldl
  , foldlWithId
  , foldr
  , foldrWithId
  , filter
  , filterWithId
  , partition
  , partitionWithId
  , union
  , intersect
  , diff
  , merge
  , dict
  )

{-|  A Set with a built in id generator. These ids are used as keys for the Dict

# Dictionaries

@docs IdSet
@docs IdGenerator

# Build

@docs empty
@docs singleton
@docs insert
@docs insertWithId
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
@docs mapWithId
@docs foldl
@docs foldlWithId
@docs foldr
@docs foldrWithId
@docs filter
@docs filterWithId
@docs partition
@docs partitionWithId

# Combine

@docs union
@docs intersect
@docs diff
@docs merge

# Helpers

@docs dict
-}

import Dict exposing (Dict)
import Util exposing (..)

{-| A set of items with a built in id generator
-}
type IdSet seed id item =
  IdSet
    (Dict id item)
    (IdGenerator seed id)
    seed

{-| A method to generate new ids
-}
type alias IdGenerator seed id = seed -> (id, seed)

{-| Creates an empty set initialized with the given IdGenerator and initial seed
-}
empty : IdGenerator seed id -> seed -> IdSet seed id item
empty idGenerator initialSeed =
  IdSet
    Dict.empty
    idGenerator
    initialSeed

{-| Creates a singleton set using the given IdGenerator and initial seed
-}
singleton: IdGenerator seed comparable -> seed -> item -> IdSet seed comparable item
singleton idGenerator seed item =
    empty idGenerator seed
      |> insert item

{-| Inserts an item into the set
-}
insert : item -> IdSet seed comparable item -> IdSet seed comparable item
insert = always >> insertWithId

{-| Inserts an item into the set. The id that will be used as a key for the
item is added as extra input
-}
insertWithId : (comparable -> item) -> IdSet seed comparable item -> IdSet seed comparable item
insertWithId fn idSet =
  let
    (dict, idGenerator, seed) = unwrap idSet
    (id, newSeed) = idGenerator seed
    item = fn id
  in
    IdSet
      (Dict.insert id item dict)
      idGenerator
      newSeed

{-| Update the value of an item with the given index -}
update
  :  comparable
  -> (Maybe item -> Maybe item)
  -> IdSet seed comparable item
  -> IdSet seed comparable item
update id fn =
  mapDict
    (Dict.update id fn)

{-| Remove a value from the set with the given id. If there is no entry with
such an id, no changes are made
-}
remove : comparable -> IdSet seed comparable item -> IdSet seed comparable item
remove id =
  mapDict
    (Dict.remove id)

{-| Determines if the set is empty
-}
isEmpty : IdSet seed id item -> Bool
isEmpty = wrap Dict.isEmpty

{-| Determines if an id is in the set
-}
member : comparable -> IdSet seed comparable item -> Bool
member item = wrap (Dict.member item)

{-| Get the item associated with the id. If the item is not in the set, it
returns Nothing.
-}
get : comparable -> IdSet seed comparable item -> Maybe item
get = wrap << Dict.get

{-| Determines the number of items in a set
-}
size : IdSet seed comparable item -> Int
size = wrap Dict.size

-- LISTS

{-| Get a list of keys (the ordinal values) of items in the set, ordered from
lowest to highest.
-}
keys : IdSet seed comparable item -> List comparable
keys = wrap Dict.keys

{-| Get a list of values of items in the set, ordered from lowest to highest.
-}
values : IdSet seed comparable item -> List item
values = wrap Dict.values

{-| Convert a set into an association list of key-value pairs, sorted by their
ordinal values.
-}
toList : IdSet seed comparable item -> List (comparable, item)
toList = wrap Dict.toList

{-| Convert a list of items into a set using the given IdGenerator and initial
seed
-}
fromList : IdGenerator seed comparable -> seed -> List item -> IdSet seed comparable item
fromList idGenerator initialSeed items =
  List.foldl
    insert
    (empty idGenerator initialSeed)
    items


-- TRANSFORM

{-| Apply a function to all values in the set.
-}
map : (item -> item) -> IdSet seed comparable item -> IdSet seed comparable item
map fn = mapWithId (expand1 fn)

{-| Apply a function to all values in the set, with the id of each item as extra
input
-}
mapWithId
  :  (comparable -> item -> item)
  -> IdSet seed comparable item
  -> IdSet seed comparable item
mapWithId fn = mapDict (Dict.map fn)

{-| Fold over all values in the set, in order from lowest key to highest key.
-}
foldl : (item -> b -> b) -> b -> IdSet seed comparable item -> b
foldl fn = foldlWithId (expand1 fn)

{-| Fold over all values in the set, with the id of each item as extra input,
in order from lowest key to highest key.
-}
foldlWithId : (comparable -> item -> b -> b) -> b -> IdSet seed comparable item -> b
foldlWithId fn init = wrap (Dict.foldl fn init)

{-| Fold over all values in the set, in order from highest key to lowest key.
-}
foldr : (item -> b -> b) -> b -> IdSet seed comparable item -> b
foldr fn = foldrWithId (expand1 fn)

{-| Fold over all values in the set, with the id of each item as extra input,
in order from highest key to lowest key.
-}
foldrWithId : (comparable -> item -> b -> b) -> b -> IdSet seed comparable item -> b
foldrWithId fn init = wrap (Dict.foldr fn init)

{-| Filter elements from the set that do not satisfy the predicate.
-}
filter : (item -> Bool) -> IdSet seed comparable item -> IdSet seed comparable item
filter fn = filterWithId (expand1 fn)

{-| Filter elements from the set that do not satisfy the predicate. The id of
each item is added as extra input.
-}
filterWithId : (comparable -> item -> Bool) -> IdSet seed comparable item -> IdSet seed comparable item
filterWithId fn =
  mapDict
    (Dict.filter fn)

{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest. Note that this function does not return an IdSet, since the
IdGenerator cannot be split in two.
-}
partition
  :  (item -> Bool)
  -> IdSet seed comparable item
  -> (Dict comparable item, Dict comparable item)
partition fn = partitionWithId (expand1 fn)

{-| Partition a dictionary according to a predicate, with the id of each item as extra input.
The first dictionary contains all key-value pairs which satisfy the predicate,
and the second contains the rest. Note that this function does not return
IdSets, since the IdGenerator cannot be split in two.
-}
partitionWithId
  :  (comparable -> item -> Bool)
  -> IdSet seed comparable item
  -> (Dict comparable item, Dict comparable item)
partitionWithId fn = wrap (Dict.partition fn)


-- COMBINE

{-| Combine two sets. If there is a collision, preference is given to the first
set. Note that this function does not return an IdSet, since the IdGenerator
cannot be split in two.
-}
union
  :  IdSet seed comparable item
  -> IdSet seed comparable item
  -> Dict comparable item
union dictLeft dictRight =
  Dict.union (dict dictLeft) (dict dictRight)

{-| Creates a set that contains items that are contained in both sets. Note that
 this function does not return an IdSet, since the IdGenerator cannot be split
 in two.
-}
intersect
  :  IdSet seed comparable item
  -> IdSet seed comparable item
  -> Dict comparable item
intersect dictLeft dictRight =
  Dict.intersect (dict dictLeft) (dict dictRight)

{-| Creates a set that contains all items that are contained in the first set,
but not in the second set.
-}
diff
  :  IdSet seed comparable item
  -> IdSet seed comparable item
  -> Dict comparable item
diff dictLeft dictRight =
  Dict.diff (dict dictLeft) (dict dictRight)

{-| The most general way of combining two sets. You provide three accumulators
for when a given key appears:

  1. Only in the left set.
  2. In both set.
  3. Only in the right set.
You then traverse all the keys from lowest to highest, building up whatever you
want.
-}
merge
  :  (a -> result -> result)
  -> (a -> b -> result -> result)
  -> (b -> result -> result)
  -> IdSet seed comparable a
  -> IdSet seed comparable b
  -> result
  -> result
merge leftStep bothStep rightStep =
  mergeWithId
    (expand1 leftStep)
    (expand1 bothStep)
    (expand1 rightStep)

{-| The most general way of combining two sets. You provide three accumulators
for when a given key appears:

  1. Only in the left set.
  2. In both set.
  3. Only in the right set.
You then traverse all the keys from lowest to highest, building up whatever you
want.
-}
mergeWithId
  :  (comparable -> a -> result -> result)
  -> (comparable -> a -> b -> result -> result)
  -> (comparable -> b -> result -> result)
  -> IdSet seed comparable a
  -> IdSet seed comparable b
  -> result
  -> result
mergeWithId leftStep bothStep rightStep leftDict rightDict init =
  (Dict.merge
    leftStep
    bothStep
    rightStep
    (dict leftDict)
    (dict rightDict)
    init
  )

-- HELPERS

{-| Converts a `Dictset` to a `Dict`
-}
dict : IdSet seed id item -> Dict id item
dict idSet = case idSet of
  IdSet dict _ _ -> dict

-- IMPLEMENTATION DETAILS

idGenerator : IdSet seed id item -> IdGenerator seed id
idGenerator idSet =
  case idSet of
    IdSet _ idGenerator _ -> idGenerator

seed : IdSet seed id item -> seed
seed idSet =
  case idSet of
    IdSet _ _ seed -> seed

unwrap : IdSet seed id item ->
  ( Dict id item
  , IdGenerator seed id
  , seed
  )
unwrap idSet =
  case idSet of
    IdSet dict idGenerator seed -> (dict, idGenerator, seed)

wrap : (Dict id item -> b) -> IdSet seed id item -> b
wrap = (>>) dict

mapDict: (Dict id item -> Dict id item) -> IdSet seed id item -> IdSet seed id item
mapDict fn idSet =
  let
    (dict, idGenerator, seed) = unwrap idSet
  in
    IdSet
      (fn dict)
      idGenerator
      seed
