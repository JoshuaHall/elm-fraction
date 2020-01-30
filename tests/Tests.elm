module Tests exposing (fractionModule, testsModule)

import Expect exposing (Expectation)
import Fraction exposing (Fraction, create, minimumSupportedInt)
import Fuzz exposing (Fuzzer)
import Ordering
import Random
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)



-- HELPERS


{-| Swaps the order of the elements of a 2 tuple.

    swap ( 1, 2 ) == ( 2, 1 )

-}
swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )



-- GENERAL TEST HELPERS


type alias Arguments4 a b c d =
    { argument1 : a
    , argument2 : b
    , argument3 : c
    , argument4 : d
    }


fuzz4 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> String -> (a -> b -> c -> d -> Expectation) -> Test
fuzz4 fuzzer1 fuzzer2 fuzzer3 fuzzer4 description expectation =
    Test.fuzz
        (Fuzz.map4 Arguments4 fuzzer1 fuzzer2 fuzzer3 fuzzer4)
        description
        (\{ argument1, argument2, argument3, argument4 } ->
            expectation argument1 argument2 argument3 argument4
        )



-- SPECIFIC LIBRARY TEST HELPERS


{-| All ints from [-maxInt, -1] and [1, maxInt]
-}
allSupportedDenominatorInts : Fuzzer Int
allSupportedDenominatorInts =
    Fuzz.oneOf
        [ Fuzz.intRange minimumSupportedInt -1
        , Fuzz.intRange 1 Random.maxInt
        ]


allSupportedNumeratorInts : Fuzzer Int
allSupportedNumeratorInts =
    Fuzz.intRange minimumSupportedInt Random.maxInt


validFractionFuzz : String -> (Int -> Int -> Expectation) -> Test
validFractionFuzz =
    fuzz2 allSupportedNumeratorInts allSupportedDenominatorInts


validTwoFractionFuzz : String -> (Int -> Int -> Int -> Int -> Expectation) -> Test
validTwoFractionFuzz =
    fuzz4
        allSupportedNumeratorInts
        allSupportedDenominatorInts
        allSupportedNumeratorInts
        allSupportedDenominatorInts


fractionExpectation : Int -> Int -> (Fraction -> Expectation) -> Expectation
fractionExpectation numerator denominator fracExpectation =
    case create numerator denominator of
        Just frac ->
            fracExpectation frac

        Nothing ->
            Expect.fail "Should never fail"


twoFractionExpectation : Int -> Int -> Int -> Int -> (Fraction -> Fraction -> Expectation) -> Expectation
twoFractionExpectation numerator1 denominator1 numerator2 denominator2 twoFracExpectation =
    let
        nothingCase =
            Expect.fail "Should never fail"
    in
    case create numerator1 denominator1 of
        Just frac1 ->
            case create numerator2 denominator2 of
                Just frac2 ->
                    twoFracExpectation frac1 frac2

                Nothing ->
                    nothingCase

        Nothing ->
            nothingCase



-- TEST SUITES


testsModule : Test
testsModule =
    describe "The Tests module"
        [ describe "Tests.allSupportedDenominatorInts"
            [ fuzz allSupportedDenominatorInts "allSupportedDenominatorInts should never return zero" <|
                \nonZeroInt ->
                    nonZeroInt
                        |> Expect.notEqual 0
            ]
        ]


{-| Tests for functions from the Fraction module
-}
fractionModule : Test
fractionModule =
    describe "The Fraction module"
        [ describe "Fraction.fraction"
            [ validFractionFuzz "fraction should work for all denominators that are valid" <|
                \numerator denominator ->
                    create numerator denominator
                        |> Expect.notEqual Nothing
            , fuzz allSupportedNumeratorInts "fraction should always fail if the denominator is invalid" <|
                \numerator ->
                    create numerator Fraction.invalidDenominator
                        |> Expect.equal Nothing
            ]
        , describe "Fraction.getNumerator"
            [ validFractionFuzz "getNumerator should return the correct numerator" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            frac
                                |> Fraction.getNumerator
                                |> Expect.equal numerator
                        )
            ]
        , describe "Fraction.getDenominator"
            [ validFractionFuzz "getDenominator should return the correct denominator" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            frac
                                |> Fraction.getDenominator
                                |> Expect.equal denominator
                        )
            ]
        , describe "Fraction.reciprocal"
            [ test "reciprocal should work with a simple fraction" <|
                \_ ->
                    fractionExpectation
                        2
                        5
                        (\frac ->
                            frac
                                |> Fraction.reciprocal
                                |> Maybe.map Fraction.toTuple
                                |> Expect.equal (Just ( 5, 2 ))
                        )
            , fuzz2
                allSupportedDenominatorInts
                allSupportedDenominatorInts
                "reciprocal returns the correct value for all non zero numerators and denominators"
              <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            frac
                                |> Fraction.reciprocal
                                |> Maybe.map Fraction.toTuple
                                |> Maybe.map swap
                                |> Expect.equal (Just (Fraction.toTuple frac))
                        )
            , fuzz allSupportedDenominatorInts "reciprocal fails if the numerator of the provided fraction is an invalid denominator" <|
                \denominator ->
                    fractionExpectation
                        Fraction.invalidDenominator
                        denominator
                        (\frac ->
                            Fraction.reciprocal frac
                                |> Expect.equal Nothing
                        )
            ]
        , describe "Fraction.simplify"
            [ test "simplify should simplify 2/4 correctly" <|
                \_ ->
                    fractionExpectation
                        2
                        4
                        (\frac ->
                            frac
                                |> Fraction.simplify
                                |> Fraction.toTuple
                                |> Expect.equal ( 1, 2 )
                        )
            , test "simplify should simplify 46/60 correctly" <|
                \_ ->
                    fractionExpectation
                        46
                        60
                        (\frac ->
                            frac
                                |> Fraction.simplify
                                |> Fraction.toTuple
                                |> Expect.equal ( 23, 30 )
                        )
            , test "simplify should return already simplified fractions as they already are" <|
                \_ ->
                    fractionExpectation
                        3
                        7
                        (\frac ->
                            frac
                                |> Fraction.simplify
                                |> Fraction.toTuple
                                |> Expect.equal ( 3, 7 )
                        )
            , validFractionFuzz "simplify should always make the denominator positive" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            frac
                                |> Fraction.simplify
                                |> Fraction.getDenominator
                                |> Expect.greaterThan 0
                        )
            ]
        , describe "Fraction.multiply"
            [ test "multiply should work with with some simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        3
                        4
                        5
                        6
                        (\frac1 frac2 ->
                            Fraction.multiply frac1 frac2
                                |> Fraction.simplify
                                |> Fraction.toTuple
                                |> Expect.equal ( 5, 8 )
                        )
            , validTwoFractionFuzz "multiply should simply multiply the numerators and the denominators" <|
                \numerator1 denominator1 numerator2 denominator2 ->
                    twoFractionExpectation
                        numerator1
                        denominator1
                        numerator2
                        denominator2
                        (\frac1 frac2 ->
                            frac1
                                |> Fraction.multiply frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( numerator1 * numerator2, denominator1 * denominator2 )
                        )
            ]
        , describe "Fraction.divide"
            [ test "divide should work on simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        2
                        3
                        4
                        (\frac1 frac2 ->
                            Fraction.divide frac1 frac2
                                |> Maybe.map Fraction.simplify
                                |> Maybe.map Fraction.toTuple
                                |> Expect.equal (Just ( 2, 3 ))
                        )
            , fuzz3 allSupportedNumeratorInts allSupportedDenominatorInts allSupportedDenominatorInts "divide should fail on fractions whose numerators are equal to zero" <|
                \numerator1 denominator1 denominator2 ->
                    twoFractionExpectation
                        numerator1
                        denominator1
                        Fraction.invalidDenominator
                        denominator2
                        (\frac1 frac2 ->
                            frac2
                                |> Fraction.divide frac1
                                |> Expect.equal Nothing
                        )
            ]
        , describe "Fraction.add"
            [ test "add should work with simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        2
                        2
                        3
                        (\frac1 frac2 ->
                            Fraction.add frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( 7, 6 )
                        )
            , test "should add a positive fraction and a negative fraction together correctly" <|
                \_ ->
                    twoFractionExpectation
                        4
                        5
                        -3
                        5
                        (\frac1 frac2 ->
                            Fraction.add frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( 1, 5 )
                        )
            , test "should add a negative fraction and a negative fraction together correctly" <|
                \_ ->
                    twoFractionExpectation
                        -7
                        9
                        -5
                        4
                        (\frac1 frac2 ->
                            Fraction.add frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( -73, 36 )
                        )
            , validTwoFractionFuzz "add should work with any ordering of the arguments" <|
                \numerator1 denominator1 numerator2 denominator2 ->
                    twoFractionExpectation
                        numerator1
                        denominator1
                        numerator2
                        denominator2
                        (\frac1 frac2 ->
                            Fraction.add frac1 frac2
                                |> Fraction.simplify
                                |> Fraction.toTuple
                                |> Expect.equal
                                    (Fraction.add frac2 frac1
                                        |> Fraction.simplify
                                        |> Fraction.toTuple
                                    )
                        )
            ]
        , describe "Fraction.subtract"
            [ test "subtract should work with simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        2
                        2
                        3
                        (\frac1 frac2 ->
                            Fraction.subtract frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( -1, 6 )
                        )
            , test "subtract should work with simple fractions 2" <|
                \_ ->
                    twoFractionExpectation
                        2
                        3
                        1
                        2
                        (\frac1 frac2 ->
                            Fraction.subtract frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( 1, 6 )
                        )
            , test "subtract should work with simple negative fractions" <|
                \_ ->
                    twoFractionExpectation
                        4
                        5
                        -3
                        5
                        (\frac1 frac2 ->
                            Fraction.subtract frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( 7, 5 )
                        )
            , test "subtract should work with simple negative fractions 2" <|
                \_ ->
                    twoFractionExpectation
                        1
                        5
                        -9
                        10
                        (\frac1 frac2 ->
                            Fraction.subtract frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( 11, 10 )
                        )
            , test "subtract should work with cleanly divisible denominators" <|
                \_ ->
                    twoFractionExpectation
                        5
                        7
                        3
                        7
                        (\frac1 frac2 ->
                            Fraction.subtract frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( 2, 7 )
                        )
            , test "subtract should work with cleanly divisible denominators 2" <|
                \_ ->
                    twoFractionExpectation
                        3
                        7
                        5
                        7
                        (\frac1 frac2 ->
                            Fraction.subtract frac1 frac2
                                |> Fraction.toTuple
                                |> Expect.equal ( -2, 7 )
                        )
            , validTwoFractionFuzz "if the order of the fractions is swapped in subtraction, if you negate one result, the two results will be equal" <|
                \numerator1 denominator1 numerator2 denominator2 ->
                    twoFractionExpectation
                        numerator1
                        denominator1
                        numerator2
                        denominator2
                        (\frac1 frac2 ->
                            let
                                firstResult =
                                    Fraction.subtract frac1 frac2
                                        |> Fraction.simplify
                                        |> Fraction.toTuple
                                        |> Tuple.mapFirst negate
                            in
                            firstResult
                                |> Expect.equal
                                    (Fraction.subtract frac2 frac1
                                        |> Fraction.simplify
                                        |> Fraction.toTuple
                                    )
                        )
            ]
        , describe "Fraction.toFloat"
            [ test "Fraction.toFloat should work on a basic fraction" <|
                \_ ->
                    fractionExpectation
                        3
                        4
                        (\frac ->
                            frac
                                |> Fraction.toFloat
                                |> Expect.within (Expect.Absolute 0.000000001) 0.75
                        )
            ]
        , describe "Fraction.roundToNearestInt"
            [ test "roundToNearestInt should work on a basic fraction" <|
                \_ ->
                    fractionExpectation
                        8
                        3
                        (\frac ->
                            frac
                                |> Fraction.roundToNearestInt
                                |> Expect.equal 3
                        )
            ]
        , describe "Fraction.isWholeNumber"
            [ fuzz allSupportedNumeratorInts "isWholeNumber should return True if the denominator is 1 after simplification" <|
                \numerator ->
                    fractionExpectation
                        numerator
                        1
                        (\frac ->
                            frac
                                |> Fraction.isWholeNumber
                                |> Expect.true "Should always be true"
                        )
            ]
        , describe "Fraction.compare"
            [ test "compare should return EQ for equal fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        2
                        2
                        4
                        (\frac1 frac2 ->
                            Fraction.compare frac1 frac2
                                |> Expect.equal EQ
                        )
            , let
                middleNum =
                    0
              in
              fuzz3
                (Fuzz.intRange minimumSupportedInt middleNum)
                (Fuzz.intRange (middleNum + 1) Random.maxInt)
                (Fuzz.intRange 1 Random.maxInt)
                "Fraction.compare should correctly compare a larger and smaller fraction"
              <|
                \lesserNumerator greaterNumerator denominator ->
                    twoFractionExpectation
                        lesserNumerator
                        denominator
                        greaterNumerator
                        denominator
                        (\lesserFrac greaterFrac ->
                            Fraction.compare lesserFrac greaterFrac
                                |> Expect.equal LT
                        )
            ]
        , describe "Fraction.sortFractions"
            [ test "sortFractions should correctly sort some simple fractions" <|
                \_ ->
                    let
                        maybeFractions =
                            Maybe.map5
                                (\frac1 frac2 frac3 frac4 frac5 -> [ frac1, frac2, frac3, frac4, frac5 ])
                                (create 1 2)
                                (create 4 5)
                                (create 3 7)
                                (create 20 12)
                                (create 1 1)
                    in
                    case maybeFractions of
                        Just fractionList ->
                            -- Sorts the fractions, then checks they are in the correct order by converting them all
                            -- to the same denominator and then comparing their numerators
                            fractionList
                                |> Fraction.sort
                                |> Fraction.convertAllToSameDenominator
                                |> List.map Fraction.getNumerator
                                |> Ordering.isOrdered Ordering.natural
                                |> Expect.true "Should order the fractions correctly"

                        Nothing ->
                            Expect.fail "If failure, check the fractions in the test definition"
            ]
        , describe "Fraction.toTuple"
            [ test "Should work with a simple fraction" <|
                \_ ->
                    fractionExpectation
                        4
                        9
                        (\frac ->
                            frac
                                |> Fraction.toTuple
                                |> Expect.equal ( 4, 9 )
                        )
            , validFractionFuzz "Should work with any valid input" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            frac
                                |> Fraction.toTuple
                                |> Expect.equal ( numerator, denominator )
                        )
            ]
        , describe "Fraction.convertToSameDenominator"
            [ test "should work with two simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        5
                        -9
                        10
                        (\frac1 frac2 ->
                            Fraction.convertToSameDenominator frac1 frac2
                                |> Tuple.mapBoth Fraction.toTuple Fraction.toTuple
                                |> Expect.equal ( ( 2, 10 ), ( -9, 10 ) )
                        )
            ]
        , describe "Fraction.equal"
            [ test "equal should work with some simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        3
                        4
                        75
                        100
                        (\frac1 frac2 ->
                            Fraction.equal frac1 frac2
                                |> Expect.true "3/4 is equal to 75/100"
                        )
            , test "equal should return false when provided unequal fractions" <|
                \_ ->
                    twoFractionExpectation
                        3
                        4
                        76
                        100
                        (\frac1 frac2 ->
                            Fraction.equal frac1 frac2
                                |> Expect.false "3/4 is not equal to 76/100"
                        )
            ]
        , describe "Fraction.isZero"
            [ test "isZero should work with a simple fraction" <|
                \_ ->
                    fractionExpectation
                        0
                        4
                        (\frac ->
                            frac
                                |> Fraction.isZero
                                |> Expect.true "0/4 is zero"
                        )
            , validFractionFuzz "isZero should work for any valid input" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            if Fraction.getNumerator frac == 0 then
                                frac
                                    |> Fraction.isZero
                                    |> Expect.true "0/n is zero"

                            else
                                frac
                                    |> Fraction.isZero
                                    |> Expect.false "(non zero int)/n is not zero"
                        )
            ]
        , describe "Fraction.isOne"
            [ test "isOne should work with a simple fraction" <|
                \_ ->
                    fractionExpectation
                        4
                        4
                        (\frac ->
                            frac
                                |> Fraction.isOne
                                |> Expect.true "4/4 is one"
                        )
            , validFractionFuzz "isOne should work for any valid input" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            if Fraction.getNumerator frac == Fraction.getDenominator frac then
                                frac
                                    |> Fraction.isOne
                                    |> Expect.true "n/n is one"

                            else
                                frac
                                    |> Fraction.isOne
                                    |> Expect.false "x/y is not one when x != y"
                        )
            ]
        , describe "Fraction.isNegativeOne"
            [ test "isNegativeOne should work with a simple fraction" <|
                \_ ->
                    fractionExpectation
                        4
                        -4
                        (\frac ->
                            frac
                                |> Fraction.isNegativeOne
                                |> Expect.true "4/-4 is negative one"
                        )
            , validFractionFuzz "isNegativeOne should work for any valid input" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            if negate (Fraction.getNumerator frac) == Fraction.getDenominator frac then
                                frac
                                    |> Fraction.isNegativeOne
                                    |> Expect.true "-n/n is negative one"

                            else
                                frac
                                    |> Fraction.isNegativeOne
                                    |> Expect.false "x/y is not one when -x != y"
                        )
            ]
        ]
