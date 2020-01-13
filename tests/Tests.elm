module Tests exposing (fractionModule, testsModule)

import Expect exposing (Expectation)
import Fraction exposing (Fraction, fraction, supportedMinInt)
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



-- SPECIFIC TEST HELPERS


{-| All ints from [-maxInt, -1] and [1, maxInt]
-}
allSupportedIntsButZero : Fuzzer Int
allSupportedIntsButZero =
    Fuzz.oneOf
        [ Fuzz.intRange supportedMinInt -1
        , Fuzz.intRange 1 Random.maxInt
        ]


allSupportedInts : Fuzzer Int
allSupportedInts =
    Fuzz.intRange supportedMinInt Random.maxInt


validFractionFuzz : String -> (Int -> Int -> Expectation) -> Test
validFractionFuzz =
    fuzz2 allSupportedInts allSupportedIntsButZero


fractionExpectation : Int -> Int -> (Fraction -> Expectation) -> Expectation
fractionExpectation numerator denominator fracExpectation =
    case fraction numerator denominator of
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
    case fraction numerator1 denominator1 of
        Just frac1 ->
            case fraction numerator2 denominator2 of
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
        [ describe "Tests.allSupportedIntsButZero"
            [ fuzz allSupportedIntsButZero "allSupportedIntsButZero should never return zero" <|
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
                    fraction numerator denominator
                        |> Expect.notEqual Nothing
            , fuzz allIntsButRandomMinInt "fraction should always fail if the denominator is invalid" <|
                \numerator ->
                    fraction numerator Fraction.invalidDenominator
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
            [ fuzz2 allSupportedIntsButZero allSupportedIntsButZero "reciprocal returns the correct value for all non zero numerators and denominators" <|
                \numerator denominator ->
                    fractionExpectation
                        numerator
                        denominator
                        (\frac ->
                            let
                                reciprocal =
                                    frac
                                        |> Fraction.reciprocal
                            in
                            case reciprocal of
                                Just validFrac ->
                                    validFrac
                                        |> Fraction.toTuple
                                        |> swap
                                        |> Expect.equal (Fraction.toTuple frac)

                                Nothing ->
                                    Expect.fail "Should always fail"
                        )
            , fuzz allSupportedIntsButZero "reciprocal fails if the numerator of the provided fraction is invalid" <|
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
            [ fuzz4 allIntsButRandomMinInt allSupportedIntsButZero allIntsButRandomMinInt allSupportedIntsButZero "multiply should simply multiply the numerators and the denominators" <|
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
            [ fuzz3 allIntsButRandomMinInt allSupportedIntsButZero allSupportedIntsButZero "divide should fail on fractions whose numerators are equal to zero" <|
                \numerator1 denominator1 denominator2 ->
                    twoFractionExpectation
                        numerator1
                        denominator1
                        0
                        denominator2
                        (\frac1 frac2 ->
                            frac2
                                |> Fraction.divide frac1
                                |> Expect.equal Nothing
                        )
            , test "divide should work on simple fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        2
                        3
                        4
                        (\frac1 frac2 ->
                            case Fraction.divide frac1 frac2 of
                                Just validFrac ->
                                    validFrac
                                        |> Fraction.simplify
                                        |> Fraction.toTuple
                                        |> Expect.equal ( 2, 3 )

                                Nothing ->
                                    Expect.fail "Should never fail"
                        )
            ]
        , describe "Fraction.isWholeNumber"
            [ fuzz allIntsButRandomMinInt "isWholeNumber should return True if the denominator is 1" <|
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
        , describe "Fraction.compareFractions"
            [ test "compareFractions should return EQ for equal fractions" <|
                \_ ->
                    twoFractionExpectation
                        1
                        2
                        2
                        4
                        (\frac1 frac2 ->
                            Fraction.compareFractions frac1 frac2
                                |> Expect.equal EQ
                        )
            , let
                middleNum =
                    0
              in
              fuzz3
                (Fuzz.intRange supportedMinInt middleNum)
                (Fuzz.intRange (middleNum + 1) Random.maxInt)
                (Fuzz.intRange 1 Random.maxInt)
                "Fraction.compareFractions should correctly compare a larger and smaller fraction"
              <|
                \lesserNumerator greaterNumerator denominator ->
                    twoFractionExpectation
                        lesserNumerator
                        denominator
                        greaterNumerator
                        denominator
                        (\lesserFrac greaterFrac ->
                            Fraction.compareFractions lesserFrac greaterFrac
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
                                (fraction 1 2)
                                (fraction 4 5)
                                (fraction 3 7)
                                (fraction 20 12)
                                (fraction 1 1)
                    in
                    case maybeFractions of
                        Just fractionList ->
                            -- Sorts the fractions, then checks they are in the correct order by converting them all
                            -- to the same denominator and then comparing their numerators
                            fractionList
                                |> Fraction.sortFractions
                                |> Fraction.convertAllToSameDenominator
                                |> List.map Fraction.getNumerator
                                |> Ordering.isOrdered Ordering.natural
                                |> Expect.true "Should order the fractions correctly"

                        Nothing ->
                            Expect.fail "If failure, check the fractions in the test definition"
            ]
        ]
