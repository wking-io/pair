module Pair exposing
    ( Pair(..)
    , branch, branchWith
    , fst, snd, merge
    , map, map2, map3, map4, map5, map6, mapBoth, extend, swap
    , andThen
    , andMap
    , maybeTraverse, resultTraverse, taskTraverse
    , equals
    )

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.


# Definition

@docs Pair


# Constructor Functions

@docs branch, branchWith


# Extracting Functions

@docs fst, snd, merge


# Mapping Functions

@docs map, map2, map3, map4, map5, map6, mapBoth, extend, swap


# Chaining Functions

@docs andThen


# Applicative Functions

@docs andMap


# Traversable Functions

@docs maybeTraverse, resultTraverse, taskTraverse


# Boolean Functions

-}


{-| Represent two values that can computed in parallel. Similar to `Maybe` and `Result` it has a natural bias to the type on the right. However, it also includes functions like [`Pair.mapBoth`](Pair#mapBoth) to map functions to both types.

    -- Branching a number, running separate functions over each value, and merging them back into one value.
    computeThenCombine :: Int -> Int
    computeThenCombine int =
      int
        |> Pair.branchWith ((+) 2) ((+) 4)
        |> Pair.mapBoth ((*) 2) ((*) 4)
        |> Pair.merge (+)

    computeThenCombine 2 -- > 32

    computeThenCombine 3 -- > 38

-}
type Pair a b
    = Pair a b


branch : a -> Pair a a
branch a =
    Pair a a


branchWith : (a -> b) -> (a -> c) -> a -> Pair b c
branchWith f g a =
    Pair (f a) (g a)


fst : Pair a b -> a


snd : Pair a b -> b


merge : (a -> b -> c) -> Pair a b -> c


map : (b -> c) -> Pair a b -> Pair a c


map2 : (b -> d -> e) -> Pair a b -> Pair c d -> Pair a e


map3 : (b -> d -> f -> g) -> Pair a b -> Pair c d -> Pair e f -> Pair a g


map4 : (b -> d -> f -> h -> i) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair a i


map5 : (b -> d -> f -> h -> j -> k) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair a k


map6 : (b -> d -> f -> h -> j -> l -> m) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair k l -> Pair a m


mapBoth : (a -> c) -> (b -> d) -> Pair a b -> Pair c d


mapBoth2 : (a -> c -> e) -> (b -> d -> f) -> Pair a b -> Pair c d -> Pair e f


mapBoth3 : (a -> c -> e -> g) -> (b -> d -> f -> h) -> Pair a b -> Pair c d -> Pair e f -> Pair g h


mapBoth4 : (a -> c -> e -> g -> i) -> (b -> d -> f -> h -> j) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j


mapBoth5 : (a -> c -> e -> g -> i -> k) -> (b -> d -> f -> h -> j -> l) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair k l


mapBoth6 : (a -> c -> e -> g -> i -> k -> m) -> (b -> d -> f -> h -> j -> l -> n) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair k l -> Pair m n


extend : (( a, b ) -> c) -> Pair a b -> Pair a c


swap : (a -> c) -> (b -> d) -> Pair a b -> Pair d c


andThen : (b -> Pair a c) -> Pair a b -> Pair a c


andMap : Pair a b -> Pair a (b -> c) -> Pair a c


maybeTraverse : Pair a (Maybe b) -> Maybe (Pair a b)
maybeTraverse (Pair a maybe) =
    Maybe.map (\b -> Pair a b) maybe


resultTraverse : Pair a (Result err b) -> Result err (Pair a b)
resultTraverse (Pair a result) =
    Result.map (\b -> Pair a b) result


taskTraverse : Pair a (Task err b) -> Task err (Pair a b)
taskTraverse (Pair a task) =
    Task.map (\b -> Pair a b) task


equals : (( a, b ) -> ( c, d ) -> Bool) -> Pair a b -> Pair c d -> Bool
