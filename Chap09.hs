myMap func [] = []
myMap func (x : xs) = func x : myMap func xs

myFilter func [] = []
myFilter func (x : xs)
  | func x = x : myFilter func xs
  | otherwise = myFilter func xs

myRemove func [] = []
myRemove func (x : xs)
  | func x = myRemove func xs
  | otherwise = x : myRemove func xs

myProduct :: (Foldable t, Num a) => t a -> a
myProduct = foldl (*) 1

testFoldl = foldl (\acc x -> x : acc) [] "tim"

testFoldl' = foldl (flip (:)) [] "tim"

testFoldr = foldr (\x acc -> x : acc) [] "tim"

testFoldr' = foldr (:) [] "tim"

firstHalfString s =
  take pickLength s
  where
    pickLength = length s `div` 2

dropLength s
  | even sLen = halfLength
  | otherwise = halfLength + 1
  where
    sLen = length s
    halfLength = sLen `div` 2

secondHalfReversed s =
  reverse secondHalfString
  where
    secondHalfString = drop (dropLength s) s

isPalindrome :: [Char] -> Bool
isPalindrome s = firstHalfString s == secondHalfReversed s

--   where
--     secondHalfString = drop (if even pickLength then pickLength else pickLength + 1) s
--     reverseHalfString = reverse secondHalfString
