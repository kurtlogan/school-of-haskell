module Filter where

data List t = Empty | Cons t (List t)
  deriving Show

keepIfEven :: List Integer -> List Integer
keepIfEven xs = keep xs even

keep :: List t -> (t -> Bool) -> List t
keep Empty _ = Empty
keep (Cons x xs) f
  | f x       = Cons x $ keep xs f
  | otherwise = keep xs f

mapList :: List a -> (a -> b) -> List b
mapList Empty _ = Empty
mapList (Cons x xs) f = Cons (f x) (mapList xs f)

myIntList :: List Integer
myIntList = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ Cons 5 $ Cons 6 Empty


myStringList :: List String
myStringList = Cons "a" $ Cons "babs" $ Cons "zzzzz" $ Cons "reallllly" Empty
