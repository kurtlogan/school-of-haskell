data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

data FailableDouble = Failure
                    | OK Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

data Person = Person String Int Thing
  deriving Show

getAge :: Person -> Int
getAge (Person _ a _) = a

describe :: Person -> String
describe p@(Person n _ _) = "The name of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ Ship) = n ++ " you're my kind of person!"
checkFav (Person n _ _) = n ++ " your taste sucks!"

data Patterns = Patterns1 Int String
              | Patterns2 Int
              | Patterns3 String
              | Patterns4

getString :: Patterns -> String
getString (Patterns1 _ s) = s
getString (Patterns3 s) = s
getString _ = ""

getInt :: Patterns -> Int
getInt (Patterns1 n _) = n
getInt (Patterns2 n) = n
getInt _ = 0

hasValues :: Patterns -> Bool
hasValues Patterns4 = False
hasValues _ = True

data IntList = Empty | Cons Int IntList deriving Show
data FailureInt = Fail | OkDokay Int deriving Show

intListLength :: IntList -> Int
intListLength Empty = 0
intListLength (Cons x xs) = 1 + intListLength xs

intListHead :: IntList -> FailureInt
intListHead Empty = Fail
intListHead (Cons n _) = OkDokay n

intListTail :: IntList -> IntList
intListTail Empty = Empty
intListTail (Cons _ xs) = xs

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1  $ Node (Leaf 'y') 2 (Leaf 'z')
