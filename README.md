# elm-fraction

This library provides a safe and simple API to deal with fractions.

Examples:

```elm
Fraction.create 1 2
    |> Maybe.map Fraction.toFloat == Just 0.5
```

```elm
Fraction.create 24 37
    |> Maybe.map Fraction.simplify |> Maybe.map Fraction.toTuple == Just (24, 37)
```
