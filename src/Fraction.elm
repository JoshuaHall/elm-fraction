module Fraction exposing
    ( Fraction
    , add
    , compareFractions
    , convertAllToSameDenominator
    , divide
    , fraction
    , fractionToFloat
    , getDenominator
    , getNumerator
    , invalidDenominator
    , isWholeNumber
    , multiply
    , reciprocal
    , roundToNearestInt
    , simplify
    , sortFractions
    , subtract
    , supportedMinInt
    , toTuple
    )

import Arithmetic exposing (gcd, lcm)
import Random


type Fraction
    = Fraction Int Int


{-| The supported minimum integer that should be used with the Fraction module.
This is because `negate Random.minInt` is greater than `Random.maxInt`, so Fraction.simplify doesn't work correctly.
-}
supportedMinInt : Int
supportedMinInt =
    Random.minInt + 1


invalidDenominator : Int
invalidDenominator =
    0


fraction : Int -> Int -> Maybe Fraction
fraction numerator denominator =
    if denominator == invalidDenominator || numerator < supportedMinInt || numerator < supportedMinInt then
        Nothing

    else
        Just <| Fraction numerator denominator


getNumerator : Fraction -> Int
getNumerator (Fraction numerator _) =
    numerator


getDenominator : Fraction -> Int
getDenominator (Fraction _ denominator) =
    denominator


reciprocal : Fraction -> Maybe Fraction
reciprocal (Fraction numerator denominator) =
    fraction denominator numerator


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


multiply : Fraction -> Fraction -> Fraction
multiply (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) =
    Fraction (numerator1 * numerator2) (denominator1 * denominator2)


divide : Fraction -> Fraction -> Maybe Fraction
divide frac1 frac2 =
    Maybe.map (multiply frac1) <| reciprocal frac2


add : Fraction -> Fraction -> Fraction
add frac1 frac2 =
    addSubtractFraction (+) frac1 frac2


subtract : Fraction -> Fraction -> Fraction
subtract frac1 frac2 =
    addSubtractFraction (-) frac1 frac2


fractionToFloat : Fraction -> Float
fractionToFloat (Fraction numerator denominator) =
    toFloat numerator / toFloat denominator


isWholeNumber : Fraction -> Bool
isWholeNumber frac =
    let
        simplified =
            simplify frac
    in
    getDenominator simplified == 1


roundToNearestInt : Fraction -> Int
roundToNearestInt frac =
    round (fractionToFloat frac)


sortFractions : List Fraction -> List Fraction
sortFractions =
    List.sortWith compareFractions


compareFractions : Fraction -> Fraction -> Order
compareFractions frac1 frac2 =
    let
        ( fraction1, fraction2 ) =
            convertToSameDenominator frac1 frac2

        fraction1Numerator =
            getNumerator fraction1

        fraction2Numerator =
            getNumerator fraction2
    in
    if fraction1Numerator > fraction2Numerator then
        GT

    else if fraction1Numerator == fraction2Numerator then
        EQ

    else
        LT


toTuple : Fraction -> ( Int, Int )
toTuple (Fraction numerator denominator) =
    ( numerator, denominator )



-- HELPERS


convertToSameDenominator : Fraction -> Fraction -> ( Fraction, Fraction )
convertToSameDenominator (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) =
    let
        fracLcm =
            lcm denominator1 denominator2
    in
    ( Fraction (numerator1 * denominator2) fracLcm, Fraction (numerator2 * denominator1) fracLcm )


convertAllToSameDenominator : List Fraction -> List Fraction
convertAllToSameDenominator fractions =
    let
        denominators =
            List.map (\(Fraction _ denominator) -> denominator) fractions

        lcmDenominator =
            lcmMultipleNumbers denominators
    in
    fractions
        |> List.map
            (\(Fraction numerator denominator) ->
                Fraction ((lcmDenominator // denominator) * numerator) lcmDenominator
            )


addSubtractFraction : (Int -> Int -> Int) -> Fraction -> Fraction -> Fraction
addSubtractFraction operator frac1 frac2 =
    let
        denominator1 =
            getDenominator frac1

        denominator2 =
            getDenominator frac2
    in
    if denominator1 |> isDivisibleBy denominator2 then
        fractionCleanlyDivisibleAddSubtract operator frac1 frac2

    else if denominator2 |> isDivisibleBy denominator1 then
        fractionCleanlyDivisibleAddSubtract operator frac2 frac1

    else
        Fraction
            (operator (getNumerator frac1 * denominator2) (getNumerator frac2 * denominator1))
            (lcm denominator1 denominator2)


fractionCleanlyDivisibleAddSubtract : (Int -> Int -> Int) -> Fraction -> Fraction -> Fraction
fractionCleanlyDivisibleAddSubtract operator (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) =
    Fraction
        (operator numerator1 (numerator2 * (denominator1 // denominator2)))
        denominator1


isDivisibleBy : Int -> Int -> Bool
isDivisibleBy num1 num2 =
    modBy num1 num2 == 0


{-| Takes the LCM of multiple numbers (ideal for 3+ numbers)
-}
lcmMultipleNumbers : List Int -> Int
lcmMultipleNumbers =
    List.foldr lcm 1
