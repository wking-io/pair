module Pair exposing
    ( Pair
    , from, branch, branchWith
    , left, right, merge
    , map, andMap, map2, mapBoth, andMapBoth, mapBoth2, extend, modify, swap
    , andThen, andThenBoth
    , mapMaybe, unwrapMaybe, mapResult, unwrapResult, mapTask, unwrapTask
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

@docs mapMaybe, unwrapMaybe, mapResult, unwrapResult, mapTask, unwrapTask


# Boolean Functions

@docs equals

-}

import Task exposing (Task)


{-| Represent two values that can computed in parallel. Similar to `Maybe` and `Result` it has a natural bias to the type on the right. However, it also includes functions like [`mapBoth`](Pair#mapBoth) to map functions to both types.

    computeThenCombine :: Int -> Int
    computeThenCombine int =
        int
            |> branch -- Split value into Pair
            |> mapBoth ((+) 2) ((*) 4) -- Map both sides
            |> merge (+) -- Combine both sides

    computeThenCombine 2 -- > 12
    computeThenCombine 3 -- > 17

-}
type Pair a b
    = Pair a b


{-| Construct a `Pair` from two values of any type.

    from [ "Elm" ] "Is the best"
    -- > Pair [ "Elm" ] "Is the best"

-}
from : a -> b -> Pair a b
from =
    Pair


{-| Construct a `Pair` by splitting one value of any type.

    branch [ "Elm" ]
    -- > Pair [ "Elm" ] [ "Elm" ]

-}
branch : a -> Pair a a
branch a =
    from a a


{-| Construct a `Pair` by splitting one value of any type and mapping the passed in functions for each side. Think of this as composing [`branch`](Pair#branch) and [`mapBoth`](Pair#mapBoth).

    branchWith String.reverse String.length "Elm"
    -- > Pair "mlE" 3

-}
branchWith : (a -> b) -> (a -> c) -> a -> Pair b c
branchWith lft rht a =
    from (lft a) (rht a)


{-| Get the value on the left side of the `Pair`.

    from 2 4
        |> mapBoth ((+) 2) ((+) 2)
        |> left
    -- > 3

-}
left : Pair a b -> a
left (Pair a b) =
    a


{-| Get the value on the right side of the `Pair`.

    from 2 4
        |> mapBoth ((+) 1) ((+) 1)
        |> left
    -- > 5

-}
right : Pair a b -> b
right (Pair a b) =
    b


{-| Get a value from a `Pair` by passing both sides of the `Pair` into one function.

    from 2 4
        |> merge (+)
    -- > 6

-}
merge : (a -> b -> c) -> Pair a b -> c
merge func (Pair a b) =
    func a b


{-| Apply a function to the right side of a `Pair`.

    branch 4
        |> map sqrt
    -- > Pair 4 2

-}
map : (b -> c) -> Pair a b -> Pair a c
map func (Pair a b) =
    from a (func b)


{-| Apply a functions that is on the right side of a `Pair` to the right value of another `Pair`. This is how to build [`map2`](Pair#map2) and can be used to build `mapN` that can take as many `Pair`s as you want.

    branch 0 (-)
        |> andMap (from 4 8)
        |> andMap (from 2 4)
    -- > Pair 2 4

-}
andMap : Pair a b -> Pair c (b -> d) -> Pair a d
andMap (Pair a b) (Pair c func) =
    Pair a (func b)


{-| Apply a function that takes two values to the right side of two `Pair`s.

    map2 max (from 8 6) (from 2 4)
    -- > Pair 2 6

-}
map2 : (b -> d -> e) -> Pair a b -> Pair c d -> Pair c e
map2 func pair1 pair2 =
    from () func
        |> andMap pair1
        |> andMap pair2


{-| Apply a function to both the left and the right side of a `Pair`.

    branch 4
        |> mapBoth ((\*) 2) sqrt
    -- > Pair 8 2

-}
mapBoth : (a -> c) -> (b -> d) -> Pair a b -> Pair c d
mapBoth lft rht (Pair a b) =
    from (lft a) (rht b)


{-| Apply functions that are on both sides of a `Pair` to the values of another `Pair`. This is how to build [`mapBoth2`](Pair#mapBoth2) and can be used to build `mapBothN` that can take as many `Pair`s as you want.

    branch (+) (-)
        |> andMapBoth (from 4 8)
        |> andMapBoth (from 2 4
     -- > Pair 6 4

-}
andMapBoth : Pair a b -> Pair (a -> c) (b -> d) -> Pair c d
andMapBoth (Pair a b) (Pair lft rgt) =
    from (lft a) (rgt b)


{-| Apply a function that takes two values to both the left and the right side of two `Pair`s.

    mapBoth2 min max (from 2 4) (from 4 6)
    -- > Pair 2 6

-}
mapBoth2 : (a -> c -> e) -> (b -> d -> f) -> Pair a b -> Pair c d -> Pair e f
mapBoth2 lft rht pair1 pair2 =
    from lft rht
        |> andMapBoth pair1
        |> andMapBoth pair2


{-| Apply a function that will be passed both the left and right values of a `Pair` to transform the right side of that `Pair`.

    branch 4
        |> extend (+)
    -- > Pair 4 8

-}
extend : (a -> b -> c) -> Pair a b -> Pair a c
extend f (Pair a b) =
    from a (f a b)


{-| Apply a function that will be passed both the left and right values of a `Pair` to transform the left side of that `Pair`.

    branch 4
        |> extend (+)
    -- > Pair 8 4

-}
modify : (a -> b -> c) -> Pair a b -> Pair c b
modify f (Pair a b) =
    from (f a b) b


{-| Swap both sides of a `Pair`.

    from "Left" "Right"
        |> swap
    -- > Pair "Right" "Left"

-}
swap : Pair a b -> Pair b a
swap (Pair a b) =
    from b a


{-| Swap both sides of a `Pair` mapping each side with the passed in function first. Think of this as [`mapBoth`](Pair#mapBoth) then [`swap`](Pair#swap).

    from "Left" "Right"
        |> swapWith String.length String.toUpper
    -- > Pair "RIGHT" 4

-}
swapWith : (a -> c) -> (b -> d) -> Pair a b -> Pair d c
swapWith lft rht (Pair a b) =
    from (rht b) (lft a)


{-| Map over a `Pair` with a function that takes in the right value and returns a new `Pair`. The left value returned from the passed in function will replace the previous left value. If you want to have control over how the left value changes check out [`andThenBoth`](Pair#andThenBoth).

    -- Reverse a string but save the original state
    reverseSave : String -> Pair String String
    reverseSave str =
        from str (String.reverse str)

    from "Left" "Right"
        |> andThen reverseSave
    -- > Pair "Right" "thgiR"

-}
andThen : (b -> Pair c d) -> Pair a b -> Pair c d
andThen func (Pair a b) =
    func b


{-| Map over a `Pair` with a function that takes in the left and right values and returns a new `Pair`.

    -- Multiply two numbers and keep track of original values
    multipleSave : Int -> Int -> Pair (List Int) Int
    multiplySave a b =
        from [ a, b ] (a * b)


    from 5 8
        |> andThenBoth multiplySave
    -- > Pair [ 5, 8 ] 40

-}
andThenBoth : (a -> b -> Pair c d) -> Pair a b -> Pair c d
andThenBoth func (Pair a b) =
    func a b


{-| Map over the right value in a `Pair` with a function that returns a `Maybe` and return a `Maybe` with the mapped `Pair` inside instead a `Pair` with a `Maybe` inside.

    -- With regular map.
    branch [ 1, 2, 3 ]
        |> map List.head -- > Pair [ 1, 2, 3 ] (Just 1)

    -- With maybeMap.
    branch [ 1, 2, 3 ]
        |> mapMaybe List.head -- > Just (Pair [ 1, 2, 3 ] 1)

-}
mapMaybe : (b -> Maybe c) -> Pair a b -> Maybe (Pair a c)
mapMaybe func ((Pair a b) as pair) =
    func b
        |> Maybe.map (from a)


{-| Works the same as [`mapMaybe`](Pair#mapMaybe) but with functions that return a `Result`.
-}
mapResult : (b -> Result err c) -> Pair a b -> Result err (Pair a c)
mapResult func ((Pair a b) as pair) =
    func b
        |> Result.map (from a)


{-| Works the same as [`mapMaybe`](Pair#mapMaybe) but with functions that return a `Task`.
-}
mapTask : (b -> Task err c) -> Pair a b -> Task err (Pair a c)
mapTask func ((Pair a b) as pair) =
    func b
        |> Task.map (from a)


{-| Unwrap a `Pair` whose right value is a `Maybe` to be a `Maybe` whose value is a `Pair`. This is just an alias for using [`mapMaybe`](Pair#mapMaybe) with the `identity` function

    branch [ 1, 2, 3 ]   -- > Pair [ 1, 2, 3] [ 1, 2, 3]
        |> map List.head -- > Pair [ 1, 2, 3] (Just 1)
        |> unwrapMaybe   -- > Just (Pair [ 1, 2, 3 ] 1)

-}
unwrapMaybe : Pair a (Maybe b) -> Maybe (Pair a b)
unwrapMaybe =
    mapMaybe identity


{-| Same as [`unwrapMaybe`](Pair#unwrapMaybe) except it unwraps a `Result`.
-}
unwrapResult : Pair a (Result err b) -> Result err (Pair a b)
unwrapResult =
    mapResult identity


{-| Same as [`unwrapMaybe`](Pair#unwrapMaybe) except it unwraps a `Task`.
-}
unwrapTask : Pair a (Task err b) -> Task err (Pair a b)
unwrapTask =
    mapTask identity


{-| Compare two `Pair` types for equality based on passed in predicate function.

    -- Basic comparison function
    basic : Int -> Int -> Int -> Int -> Bool
    basic a1 b1 a2 b2 =
        a1 == a2 && b1 == b2

    equals basic (from 1 2) (from 1 2) -- > True
    equals basic (from 1 2) (from 1 3) -- > False

    -- Complex comparison function
    complex : Int -> Int -> Int -> Int -> Bool
    complex a1 b1 a2 b2 =
        a1 * b1 && a2 / b2

    equals complex (from 1 2) (from 4 2) -- > True
    equals complex (from 1 2) (from 6 2) -- > False

-}
equals : (a1 -> b1 -> a2 -> b2 -> Bool) -> Pair a1 b1 -> Pair a2 b2 -> Bool
equals pred (Pair a1 b1) (Pair a2 b2) =
    pred a1 b1 a2 b2
