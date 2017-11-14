module PatternMatching where

-- Type of 'a' must be an integral (Int or Integer)
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky _ = " Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "This is an empty list"
tell [x] = "This list contains 1 item: " ++ show x
tell [x, y] = "This list contains 2 items: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list contains many items, the first 2 are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

capital :: String -> String
capital "" = "Empty string, Woops!"
capital a@(x:_) = "The first letter of " ++ a ++ " is " ++ [x]
