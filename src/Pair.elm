module Pair exposing
    ( Pair
    , from, branch, branchWith
    , left, right, merge
    , map, andMap, map2, mapBoth, andMapBoth, mapBoth2, extend, modify, swap
    , andThen, andThenBoth
    , maybeMap, maybeUnwrap, resultMap, resultUnwrap, taskMap, taskUnwrap
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

@docs map, andMap, map2, mapBoth, andMapBoth, mapBoth2, extend, modify, swap


# Chaining Functions

@docs andThen, andThenBoth


# Traversable Functions

@docs maybeMap, maybeUnwrap, resultMap, resultUnwrap, taskMap, taskUnwrap


# Boolean Functions

@docs equals

-}

import Task exposing (Task)


{-| Represent two values that can computed in parallel. Similar to `Maybe` and `Result` it has a natural bias to the type on the right. However, it also includes functions like [`Pair.mapBoth`](Pair#mapBoth) to map functions to both types.

    -- Branching a number, running separate functions over each value, and merging them back into one value.

    computeThenCombine :: Int -> Int
    computeThenCombine int =
        int
            |> Pair.branchWith ((+) 2) ((+) 4)
            |> Pair.mapBoth ((_) 2) ((_) 4)
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
    from a a


{-| Construct a Pair by splitting one value of any type and mapping the passed in functions for each side. Think of this as composing `branch` and `mapBoth`.

    branchWith String.reverse String.length "Elm" -- > Pair "mlE" 3

-}
branchWith : (a -> b) -> (a -> c) -> a -> Pair b c
branchWith lft rht a =
    from (lft a) (rht a)


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


{-| Apply a functions that is on the right side of a Pair to the right value of another Pair. This is how to build `map2` and can be used to build `mapN` that can take as many Pairs as you want.

    branch (+) (-) -- > Pair (number -> number -> number) (number -> number -> number)
        |> andMapBoth (from 4 8) -- > Pair (number -> number) (number -> number)
        |> andMapBoth (from 2 4) -- > Pair 6 4

-}
andMap : Pair a b -> Pair c (b -> d) -> Pair a d
andMap (Pair a b) (Pair c func) =
    Pair a (func b)


{-| Apply a function that takes two values to the right side of two pairs.

    map2 max (from 8 6) (from 2 4) -- > Pair 2 6

-}
map2 : (b -> d -> e) -> Pair a b -> Pair c d -> Pair c e
map2 func pair1 pair2 =
    from () func
        |> andMap pair1
        |> andMap pair2


{-| Apply a function to both the left and the right side of a pair.

    branch 4
        |> mapBoth ((\*) 2) sqrt -- > Pair 8 2

-}
mapBoth : (a -> c) -> (b -> d) -> Pair a b -> Pair c d
mapBoth lft rht (Pair a b) =
    from (lft a) (rht b)


{-| Apply functions that are on both sides of a Pair to the values of another Pair. This is how to build `mapBoth2` and can be used to build `mapBothN` that can take as many Pairs as you want.

    branch (+) (-) -- > Pair (number -> number -> number) (number -> number -> number)
        |> andMapBoth (from 4 8) -- > Pair (number -> number) (number -> number)
        |> andMapBoth (from 2 4) -- > Pair 6 4

-}
andMapBoth : Pair a b -> Pair (a -> c) (b -> d) -> Pair c d
andMapBoth (Pair a b) (Pair lft rgt) =
    from (lft a) (rgt b)


{-| Apply a function that takes two values to both the left and the right side of two pairs.

    mapBoth2 min max (from 2 4) (from 4 6) -- > Pair 2 6

-}
mapBoth2 : (a -> c -> e) -> (b -> d -> f) -> Pair a b -> Pair c d -> Pair e f
mapBoth2 lft rht pair1 pair2 =
    from lft rht
        |> andMapBoth pair1
        |> andMapBoth pair2


{-| Apply a function that will be passed both the left and right values of the pair to transform the right side of the Pair.

    branch 4
        |> extend (+) -- > Pair 4 8

-}
extend : (a -> b -> c) -> Pair a b -> Pair a c
extend f (Pair a b) =
    from a (f a b)


{-| Apply a function that will be passed both the left and right values of the pair to transform the left side of the Pair.

    branch 4
        |> extend (+) -- > Pair 8 4

-}
modify : (a -> b -> c) -> Pair a b -> Pair c b
modify f (Pair a b) =
    from (f a b) b


{-| Swap both sides of a Pair.

    from "Left" "Right"
        |> swap -- > Pair "Right" "Left"

-}
swap : Pair a b -> Pair b a
swap (Pair a b) =
    from b a


{-| Swap both sides of a Pair mapping each side with the passed in function first. Think of this as `mapBoth` then `swap`.

    from "Left" "Right"
        |> swapWith String.length String.toUpper -- > Pair "RIGHT" 4

-}
swapWith : (a -> c) -> (b -> d) -> Pair a b -> Pair d c
swapWith lft rht (Pair a b) =
    from (rht b) (lft a)


{-| Map over a Pair with a function that takes in the right value and returns a new Pair. The left value returned from the passed in function will replace the previous left value. If you want to have control over how the left value changes check out `andThenBoth`.

    from "Left" "Right"
        |> chain (\x -> from "newLeft" (String.reverse x)) -- > Pair "newLeft" "thgiR"

-}
andThen : (b -> Pair c d) -> Pair a b -> Pair c d
andThen func (Pair a b) =
    func b


{-| Map over a Pair with a function that takes in the left and right values and returns a new Pair.

    from "Left" "Right"
        |> chain (\x y -> from (String.length) (String.reverse x)) -- > Pair 4 "thgiR"

-}
andThenBoth : (a -> b -> Pair c d) -> Pair a b -> Pair c d
andThenBoth func (Pair a b) =
    func a b


{-| Map over the right value in a Pair with a function that returns a `Maybe` and return a `Maybe` with the mapped `Pair` inside instead a `Pair` with a `Maybe` inside.

    -- With regular map.
    branch [ 1, 2, 3 ]
        |> map List.head -- > Pair [ 1, 2, 3 ] (Just 1)

    -- With maybeMap.
    branch [ 1, 2, 3 ]
        |> maybeMap List.head -- > Just (Pair [ 1, 2, 3 ] 1)

-}
maybeMap : (b -> Maybe c) -> Pair a b -> Maybe (Pair a c)
maybeMap func ((Pair a b) as pair) =
    func b
        |> Maybe.map (\c -> Pair a c)


resultMap : (b -> Result err c) -> Pair a b -> Result err (Pair a c)
resultMap func ((Pair a b) as pair) =
    func b
        |> Result.map (\c -> Pair a c)


taskMap : (b -> Task err c) -> Pair a b -> Task err (Pair a c)
taskMap func ((Pair a b) as pair) =
    func b
        |> Task.map (\c -> Pair a c)


maybeUnwrap : Pair a (Maybe b) -> Maybe (Pair a b)
maybeUnwrap =
    maybeMap identity


resultUnwrap : Pair a (Result err b) -> Result err (Pair a b)
resultUnwrap =
    resultMap identity


taskUnwrap : Pair a (Task err b) -> Task err (Pair a b)
taskUnwrap =
    taskMap identity


equals : (a -> b -> c -> d -> Bool) -> Pair a b -> Pair c d -> Bool
equals pred (Pair a b) (Pair c d) =
    pred a b c d
