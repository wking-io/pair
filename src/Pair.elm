module Pair exposing
    ( Pair
    , from, branch, branchWith
    , left, right, merge
    , map, map2, map3, map4, map5, map6, mapBoth, extend, swap
    , andThen
    , maybeTraverse, resultTraverse, taskTraverse
    , equals
    )

{-| This library makes working with two different related types easier.


# Definition

@docs Pair


# Constructor Functions

@docs from, branch, branchWith


# Extracting Functions

@docs left, right, merge


# Mapping Functions

@docs map, map2, map3, map4, map5, map6, mapBoth, extend, swap


# Chaining Functions

@docs andThen


# Traversable Functions

@docs maybeTraverse, resultTraverse, taskTraverse


# Boolean Functions

-}

import Task exposing (Task)


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


{-| Construct a Pair from two values of any type.

    from [ "Elm" ] "Is the best" -- > Pair [ "Elm" ] "Is the best"

-}
from : a -> b -> Pair a b
from =
    Pair


{-| Construct a Pair by splittingone value of any type.

    branch [ "Elm" ] -- > Pair [ "Elm" ]

-}
branch : a -> Pair a a
branch a =
    Pair a a


{-| Construct a Pair by splitting one value of any type and mapping the passed in function over each side. Think of this as composing `branch` and `mapBoth`.

    branchWith String.reverse String.length "Elm" -- > Pair "mlE" 3

-}
branchWith : (a -> b) -> (a -> c) -> a -> Pair b c
branchWith lft rht a =
    Pair (lft a) (rht a)


{-| Get the value on the left side of the Pair.

    from 2 4
        |> mapBoth ((+) 2) ((+) 2)
        |> left -- > 3

-}
left : Pair a b -> a
left (Pair a b) =
    a


{-| Get the value on the right side of the Pair.

    from 2 4
        |> mapBoth ((+) 1) ((+) 1)
        |> left -- > 5

-}
right : Pair a b -> b
right (Pair a b) =
    b


{-| Get a value from a Pair by passing both sides of the Pair into one function.

    from 2 4
        |> merge (+) -- > 6s

-}
merge : (a -> b -> c) -> Pair a b -> c
merge func (Pair a b) =
    func a b


{-| Apply a function to the right side of the pair.

    branch 4
        |> map sqrt -- > Pair 4 2

-}
map : (b -> c) -> Pair a b -> Pair a c
map func (Pair a b) =
    Pair a (func b)


{-| Apply a function that takes two values to the right side of two pairs.

    map2 max (from 2 4) (from 4 6) -- >  Pair 2 6

-}
map2 : (b -> d -> e) -> Pair a b -> Pair c d -> Pair a e
map2 func (Pair a b) (Pair c d) =
    Pair a (func b d)


map3 : (b -> d -> f -> g) -> Pair a b -> Pair c d -> Pair e f -> Pair a g
map3 func (Pair a b) (Pair c d) (Pair e f) =
    Pair a (func b d f)


map4 : (b -> d -> f -> h -> i) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair a i
map4 func (Pair a b) (Pair c d) (Pair e f) (Pair g h) =
    Pair a (func b d f h)


map5 : (b -> d -> f -> h -> j -> k) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair a k
map5 func (Pair a b) (Pair c d) (Pair e f) (Pair g h) (Pair i j) =
    Pair a (func b d f h j)


map6 : (b -> d -> f -> h -> j -> l -> m) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair k l -> Pair a m
map6 func (Pair a b) (Pair c d) (Pair e f) (Pair g h) (Pair i j) (Pair k l) =
    Pair a (func b d f h j l)


{-| Apply a function to both the leeft and the right side of the pair.

    branch 4
        |> mapBoth ((*) 2) sqrt -- > Pair 8 2

-}
mapBoth : (a -> c) -> (b -> d) -> Pair a b -> Pair c d
mapBoth lft rht (Pair a b) =
    Pair (lft a) (rht b)


{-| Apply a function that takes two values to both the left and the right side of two pairs.

    mapBoth2 max (from 2 4) (from 4 6) -- >  Pair 2 6

-}
mapBoth2 : (a -> c -> e) -> (b -> d -> f) -> Pair a b -> Pair c d -> Pair e f
mapBoth2 lft rht (Pair a b) (Pair c d) =
    Pair (lft a c) (rht b d)


mapBoth3 : (a -> c -> e -> g) -> (b -> d -> f -> h) -> Pair a b -> Pair c d -> Pair e f -> Pair g h
mapBoth3 lft rht (Pair a b) (Pair c d) (Pair e f) =
    Pair (lft a c e) (rht b d f)


mapBoth4 : (a -> c -> e -> g -> i) -> (b -> d -> f -> h -> j) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j
mapBoth4 lft rht (Pair a b) (Pair c d) (Pair e f) (Pair g h) =
    Pair (lft a c e g) (rht b d f h)


mapBoth5 : (a -> c -> e -> g -> i -> k) -> (b -> d -> f -> h -> j -> l) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair k l
mapBoth5 lft rht (Pair a b) (Pair c d) (Pair e f) (Pair g h) (Pair i j) =
    Pair (lft a c e g i) (rht b d f h j)


mapBoth6 : (a -> c -> e -> g -> i -> k -> m) -> (b -> d -> f -> h -> j -> l -> n) -> Pair a b -> Pair c d -> Pair e f -> Pair g h -> Pair i j -> Pair k l -> Pair m n
mapBoth6 lft rht (Pair a b) (Pair c d) (Pair e f) (Pair g h) (Pair i j) (Pair k l) =
    Pair (lft a c e g i k) (rht b d f h j l)


{-| Apply a function that will be passed both the left and right values of the pair to transform the right side of the Pair.

    branch 4
        |> extend (+) -- > Pair 4 8

-}
extend : (Pair a b -> c) -> Pair a b -> Pair a c
extend f ((Pair a b) as pair) =
    Pair a (f pair)


swap : (a -> c) -> (b -> d) -> Pair a b -> Pair d c
swap lft rht (Pair a b) =
    Pair (rht b) (lft a)


andThen : (a -> Pair a c) -> Pair a b -> Pair a c
andThen func (Pair a b) =
    func b
        |> mapBoth (always a) identity


andThenWith : (a -> b -> Pair c d) -> Pair a b -> Pair c d
andThenWith func (Pair a b) =
    func a b


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
equals pred (Pair a b) (Pair c d) =
    pred ( a, b ) ( c, d )
