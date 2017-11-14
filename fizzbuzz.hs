module FizzBuzz where

main :: Int -> [String]
main x = map fizzileMaBizzle [1..x]

fizzileMaBizzle :: Int -> String
fizzileMaBizzle x
  | x `isDiffizzableBy` 15 = "FizzBuzz"
  | x `isDiffizzableBy` 3  = "Fizz"
  | x `isDiffizzableBy` 5  = "Buzz"
  | otherwise              = show x

isDiffizzableBy :: Int -> Int -> Bool
isDiffizzableBy x y = x `mod` y == 0
