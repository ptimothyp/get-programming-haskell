myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ []= []
myTake n (x:xs) = x:myTake next xs
  where next = n-1


gcd1 a b
  | remainder == 0 = b
  | otherwise = gcd1 b remainder
  where remainder = a `mod` b
