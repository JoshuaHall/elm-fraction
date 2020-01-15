# elm-fraction

This library provides a safe and simple API to deal with fractions.

Examples:

```elm
Fraction.create 1 2
    |> Maybe.map Fraction.fractionToFloat -- Just 0.5


Fraction.create 3 0
    |> Maybe.map Fraction.fractionToFloat -- Nothing


Fraction.create 8 32
    |> Maybe.map Fraction.simplify
    |> Maybe.map Fraction.toTuple -- Just (1, 4)


Fraction.createUnsafe 5 4
    |> Fraction.multiply (Fraction.createUnsafe 8 3)
    |> Fraction.simplify
    |> Fraction.getDenominator -- 3



Fraction.convertToSameDenominator
    (Fraction.createUnsafe 2 3)
    (Fraction.createUnsafe 3 5) -- (Fraction 10 15, Fraction 9 15)
```
