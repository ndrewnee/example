-- http://learnyouahaskell.com/types-and-typeclasses

module TypesAndTypeClasses
    ( factorial
    )
where

factorial :: Integer -> Integer
factorial n = product [1 .. n]
