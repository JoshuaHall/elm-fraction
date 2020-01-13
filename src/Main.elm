module Main exposing (fractionToString, main)

import Browser
import Fraction
    exposing
        ( Fraction
        , fraction
        , fractionToFloat
        , getDenominator
        , getNumerator
        , simplify
        )
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { numerator : Maybe Int
    , denominator : Maybe Int
    }


init : Model
init =
    { numerator = Nothing
    , denominator = Nothing
    }


type Msg
    = Numerator String
    | Denominator String
    | Simplify
    | Reciprocal


update : Msg -> Model -> Model
update msg model =
    case msg of
        Numerator str ->
            case String.toInt str of
                Nothing ->
                    if String.isEmpty str then
                        { model | numerator = Nothing }

                    else
                        model

                Just numerator ->
                    { model | numerator = Just numerator }

        Denominator str ->
            case String.toInt str of
                Nothing ->
                    if String.isEmpty str then
                        { model | denominator = Nothing }

                    else
                        model

                Just denominator ->
                    { model | denominator = Just denominator }

        Simplify ->
            case ( model.numerator, model.denominator ) of
                ( Just numerator, Just denominator ) ->
                    let
                        frac =
                            fraction numerator denominator
                    in
                    case frac of
                        Just parsedFraction ->
                            fractionToModel (simplify parsedFraction)

                        Nothing ->
                            model

                _ ->
                    model

        Reciprocal ->
            { numerator = model.denominator
            , denominator = model.numerator
            }


view : Model -> Html Msg
view model =
    div
        []
        [ viewInput "Numerator" (emptyIfNothing model.numerator) Numerator
        , viewInput "Denominator" (emptyIfNothing model.denominator) Denominator
        , button [ onClick Simplify ] [ text "Simplify" ]
        , button [ onClick Reciprocal ] [ text "Reciprocal" ]
        , p [] [ text (displayModelAsFloatString model) ]
        ]


fractionToString : Fraction -> String
fractionToString frac =
    let
        numerator =
            String.fromInt <| getNumerator frac

        denominator =
            String.fromInt <| getDenominator frac
    in
    "(" ++ numerator ++ " / " ++ denominator ++ ")"


displayModelAsFloatString : Model -> String
displayModelAsFloatString model =
    case modelToFloat model of
        Just float ->
            String.fromFloat float

        Nothing ->
            "Invalid fraction, can't compute a float representation."


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput placeholderText inputValue toMsg =
    input
        [ type_ "text"
        , value inputValue
        , onInput toMsg
        , placeholder placeholderText
        ]
        []



-- HELPERS


emptyIfNothing : Maybe Int -> String
emptyIfNothing maybe =
    case maybe of
        Just num ->
            String.fromInt num

        Nothing ->
            ""


modelToFloat : Model -> Maybe Float
modelToFloat model =
    Maybe.map fractionToFloat <| modelToFraction model


fractionToModel : Fraction -> Model
fractionToModel frac =
    { numerator = Just (getNumerator frac)
    , denominator = Just (getDenominator frac)
    }


modelToFraction : Model -> Maybe Fraction
modelToFraction model =
    case ( model.numerator, model.denominator ) of
        ( Just numerator, Just denominator ) ->
            fraction numerator denominator

        _ ->
            Nothing
