myMap func [] = []
myMap func (x : xs) = func x : myMap func xs

myFilter func [] = []
myFilter func (x : xs)
  | func x = x : myFilter func xs
  | otherwise = myFilter func xs
