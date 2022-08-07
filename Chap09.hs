import Data.Char
myMap func [] = []
myMap func (x : xs) = func x : myMap func xs

myFilter func [] = []
myFilter func (x : xs)
  | func x = x : myFilter func xs
  | otherwise = myFilter func xs

isPalindrome s = nospacelower == reverse nospacelower
    where
      nospacelower = filter (/= ' ') lower
      lower = map toLower s
