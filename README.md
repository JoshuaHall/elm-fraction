# elm-fraction

This library provides a safe and simple API to deal with fractions.

Examples:

```elm
Fraction.create 1 2
    |> Maybe.map Fraction.toFloat == Just 0.5


Fraction.create 3 0
    |> Maybe.map Fraction.toFloat == Nothing


Fraction.create 8 32
    |> Maybe.map Fraction.simplify
    |> Maybe.map Fraction.toTuple == Just (1, 4)


Fraction.createUnsafe 1 2
    |> Fraction.multiply (Fraction.createUnsafe 3 7)
    |> Fraction.simplify
    |> Fraction.getDenominator == 14
```
