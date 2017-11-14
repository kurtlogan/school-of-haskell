

len :: [Int] -> Int
len [] = 0
len (_:xs) = 1 + len xs

main :: IO ()
main = do
  let xs = [1..1000000]
  print (len xs)
