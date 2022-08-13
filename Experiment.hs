-- sum :: (Num n) => [n] -> n
sum' :: (Foldable t) => t  Int -> Int
sum' = foldr (+) 0


