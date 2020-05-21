module Fraction exposing
    ( Fraction
    , invalidDenominator, minimumSupportedInt
    , create, FractionCreationError(..), createWithFeedback, createUnsafe, fromTuple
    , getNumerator, getDenominator, isWholeNumber, isZero, isOne, isNegativeOne
    , reciprocal, simplify, add, subtract, multiply, divide
    , compare, equal, sort
    , convertToSameDenominator, convertAllToSameDenominator, toFloat, roundToNearestInt, toTuple
    )

{-| This library provides a safe and simple API to deal with fractions.


# Definition

@docs Fraction


# Important Numbers

@docs invalidDenominator, minimumSupportedInt


# Fraction Creation

@docs create, FractionCreationError, createWithFeedback, createUnsafe, fromTuple


# Fraction Information

@docs getNumerator, getDenominator, isWholeNumber, isZero, isOne, isNegativeOne


# Manipulating Fractions

@docs reciprocal, simplify, add, subtract, multiply, divide


# Comparing Fractions

@docs compare, equal, sort


# Converting Fractions

@docs convertToSameDenominator, convertAllToSameDenominator, toFloat, roundToNearestInt, toTuple

-}

import Arithmetic exposing (gcd, lcm)
import Random


{-| The underlying opaque type of the Fraction module. Contains a numerator and a denominator.
-}
type Fraction
    = Fraction Int Int


{-| The minimum supported integer that should be used with the Fraction module.
This is because `negate Random.minInt` is 1 greater than `Random.maxInt`, so [`simplify`](#simplify) doesn't work correctly.
-}
minimumSupportedInt : Int
minimumSupportedInt =
    Random.minInt + 1


{-| Zero is an invalid denominator value because it implies division by zero, which is mathematically undefined.
-}
invalidDenominator : Int
invalidDenominator =
    0


{-| Attempts to create a [`Fraction`](#Fraction).

    create 1 2 == Just (Fraction 1 2)

    create 1 0 == Nothing

-}
create : Int -> Int -> Maybe Fraction
create numerator denominator =
    Result.toMaybe <| createWithFeedback numerator denominator


{-| Provides feedback based on the error for the user when creating a fraction.
-}
type FractionCreationError
    = NumeratorError String
    | DenominatorError String


{-| Attempts to create a [`Fraction`](#Fraction).

    create 1 2 == Ok (Fraction 1 2)

    create 1 0 == Err (DenominatorError "Invalid denominator.")

-}
createWithFeedback : Int -> Int -> Result FractionCreationError Fraction
createWithFeedback numerator denominator =
    if denominator == invalidDenominator then
        Err <| DenominatorError "Invalid denominator."

    else if numerator < minimumSupportedInt then
        Err <| NumeratorError <| "Numerator is below " ++ String.fromInt minimumSupportedInt ++ "."

    else if denominator < minimumSupportedInt then
        Err <| DenominatorError <| "Denominator is below " ++ String.fromInt minimumSupportedInt ++ "."

    else
        Ok <| Fraction numerator denominator


{-| **WARNING**: This should be used as a last resort. This is intended for use with integer literals, not with user input.
Never trust user input.
If an invalid denominator or an out of bounds integer is supplied, weird behavior could occur.

    createUnsafe 4 3 == Fraction 4 3

-}
createUnsafe : Int -> Int -> Fraction
createUnsafe numerator denominator =
    Fraction numerator denominator


{-| Attempts to take a pair of ints and make a [`Fraction`](#Fraction).
-}
fromTuple : ( Int, Int ) -> Maybe Fraction
fromTuple ( numerator, denominator ) =
    create numerator denominator


{-| Gets the numerator.

    getNumerator (Fraction 3 5) == 3

-}
getNumerator : Fraction -> Int
getNumerator (Fraction numerator _) =
    numerator


{-| Gets the denominator.

    getDenominator (Fraction 3 5) == 5

-}
getDenominator : Fraction -> Int
getDenominator (Fraction _ denominator) =
    denominator


{-| Attempts to take the reciprocal of a [`Fraction`](#Fraction). This returns a [`Maybe`](https://package.elm-lang.org/packages/elm/core/latest/Maybe) because the
numerator could be zero and then be swapped to the denominator, which is invalid.

    reciprocal (Fraction 3 4) == Just (Fraction 4 3)

    reciprocal (Fraction 0 4) == Nothing

-}
reciprocal : Fraction -> Maybe Fraction
reciprocal (Fraction numerator denominator) =
    create denominator numerator


{-| Puts a [`Fraction`](#Fraction) in the simplest possible terms.

    simplify (Fraction 5 15) == Fraction 1 3

    simplify (Fraction 3 4) == Fraction 3 4

-}
simplify : Fraction -> Fraction
simplify (Fraction numerator denominator) =
    let
        numeratorDenominatorGcd =
            gcd numerator denominator
    in
    if denominator > 0 then
        Fraction (numerator // numeratorDenominatorGcd) (denominator // numeratorDenominatorGcd)

    else
        Fraction (negate numerator // numeratorDenominatorGcd) (negate denominator // numeratorDenominatorGcd)


{-| Multiplies two [`Fraction`](#Fraction)s to get their product. Does no simplification of the result.

    multiply (Fraction 2 3) (Fraction 3 4) == Fraction 6 12

-}
multiply : Fraction -> Fraction -> Fraction
multiply (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) =
    Fraction (numerator1 * numerator2) (denominator1 * denominator2)


{-| Divides two [`Fraction`](#Fraction)s to get their quotient. Does no simplification of the result.

    divide (Fraction 2 3) (Fraction 3 4) == Just (Fraction 8 9)

    divide (Fraction 0 2) (Fraction 4 7) == Nothing

-}
divide : Fraction -> Fraction -> Maybe Fraction
divide fraction1 fraction2 =
    Maybe.map (multiply fraction1) <| reciprocal fraction2


{-| Adds two [`Fraction`](#Fraction)s to get their sum. Does no simplification of the result.

    add (Fraction 2 3) (Fraction 1 2) == Fraction 7 6

-}
add : Fraction -> Fraction -> Fraction
add fraction1 fraction2 =
    let
        ( Fraction numerator1 denominator1, Fraction numerator2 _ ) =
            convertToSameDenominator fraction1 fraction2
    in
    Fraction
        (numerator1 + numerator2)
        denominator1


{-| Subtracts two [`Fraction`](#Fraction)s to get their difference. Does no simplification of the result.

    subtract (Fraction 5 9) (Fraction 1 2) == Fraction 1 18

-}
subtract : Fraction -> Fraction -> Fraction
subtract fraction1 fraction2 =
    let
        ( Fraction numerator1 denominator1, Fraction numerator2 _ ) =
            convertToSameDenominator fraction1 fraction2
    in
    Fraction
        (numerator1 - numerator2)
        denominator1


{-| Gets the floating point representation of the [`Fraction`](#Fraction).

    Fraction.toFloat (Fraction 1 2) == 0.5

-}
toFloat : Fraction -> Float
toFloat (Fraction numerator denominator) =
    Basics.toFloat numerator / Basics.toFloat denominator


{-| Checks if a [`Fraction`](#Fraction) is a whole number.

    isWholeNumber (Fraction 5 3) == False

    isWholeNumber (Fraction 8 1) == True

    isWholeNumber (Fraction 8 2) == True

-}
isWholeNumber : Fraction -> Bool
isWholeNumber fraction =
    let
        (Fraction _ denominator) =
            simplify fraction
    in
    denominator == 1


{-| Checks if a [`Fraction`](#Fraction) is equal to zero.

    isZero (Fraction 43 32) == False

    isZero (Fraction 0 3) == True

-}
isZero : Fraction -> Bool
isZero (Fraction numerator _) =
    numerator == 0


{-| Checks if a [`Fraction`](#Fraction) is equal to one.

    isOne (Fraction 43 32) == False

    isOne (Fraction 3 3) == True

-}
isOne : Fraction -> Bool
isOne (Fraction numerator denominator) =
    numerator == denominator


{-| Checks if a [`Fraction`](#Fraction) is equal to negative one.

    isNegativeOne (Fraction 43 32) == False

    isNegativeOne (Fraction -3 3) == True

    isNegativeOne (Fraction 3 -3) == True

-}
isNegativeOne : Fraction -> Bool
isNegativeOne (Fraction numerator denominator) =
    negate numerator == denominator


{-| Rounds a [`Fraction`](#Fraction) to the nearest integer.
-}
roundToNearestInt : Fraction -> Int
roundToNearestInt =
    toFloat >> round


{-| Returns how `fraction1` compares to `fraction2`.
-}
compare : Fraction -> Fraction -> Order
compare fraction1 fraction2 =
    let
        ( Fraction numerator1 _, Fraction numerator2 _ ) =
            convertToSameDenominator fraction1 fraction2
    in
    if numerator1 > numerator2 then
        GT

    else if numerator1 == numerator2 then
        EQ

    else
        LT


{-| Checks if `fraction1` is equal to `fraction2`
-}
equal : Fraction -> Fraction -> Bool
equal fraction1 fraction2 =
    let
        ( Fraction numerator1 _, Fraction numerator2 _ ) =
            convertToSameDenominator fraction1 fraction2
    in
    numerator1 == numerator2


{-| Sorts a `List` of [`Fraction`](#Fraction)s.
-}
sort : List Fraction -> List Fraction
sort =
    List.sortWith compare


{-| Converts a [`Fraction`](#Fraction) to a tuple pair.

    toTuple (Fraction 5 8) == ( 5, 8 )

-}
toTuple : Fraction -> ( Int, Int )
toTuple (Fraction numerator denominator) =
    ( numerator, denominator )


{-| Converts `fraction1` and `fraction2` to the same denominator.

    Maybe.map2
        (\fraction1 fraction2 -> convertToSameDenominator fraction1 fraction2)
        (create 1 2)
        (create 1 3) -- Just (Fraction 3 6, Fraction  2 6)

-}
convertToSameDenominator : Fraction -> Fraction -> ( Fraction, Fraction )
convertToSameDenominator fraction1 fraction2 =
    let
        denominator1 =
            getDenominator fraction1

        denominator2 =
            getDenominator fraction2
    in
    if denominator1 == denominator2 then
        ( fraction1, fraction2 )

    else
        let
            denominatorLcm =
                lcm denominator1 denominator2
        in
        ( Fraction (getNumerator fraction1 * (denominatorLcm // denominator1)) denominatorLcm
        , Fraction (getNumerator fraction2 * (denominatorLcm // denominator2)) denominatorLcm
        )


{-| Similar to [`convertToSameDenominator`](#convertToSameDenominator), but is ideal for use cases involving 3+ [`Fraction`](#Fraction)s.
-}
convertAllToSameDenominator : List Fraction -> List Fraction
convertAllToSameDenominator fractions =
    let
        lcmDenominator =
            fractions
                |> List.map getDenominator
                |> lcmMultipleNumbers
    in
    fractions
        |> List.map
            (\(Fraction numerator denominator) ->
                Fraction ((lcmDenominator // denominator) * numerator) lcmDenominator
            )



-- HELPERS


{-| Takes the LCM of multiple numbers (ideal for 3+ numbers).
-}
lcmMultipleNumbers : List Int -> Int
lcmMultipleNumbers =
    List.foldr lcm 1
