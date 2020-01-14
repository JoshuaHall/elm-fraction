# elm-fraction

This library provides a safe and simple API to deal with fractions.

Examples:

```elm
Fraction.create 1 2
    |> Maybe.map Fraction.toFloat == Just 0.5


Fraction.create 8 32
    |> Maybe.map Fraction.simplify |> Maybe.map Fraction.toTuple == Just (1, 4)
```
