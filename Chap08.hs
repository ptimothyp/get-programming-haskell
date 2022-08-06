-- myCycle :: [a] -> [a]
myCycle (first : rest) = first : myCycle (rest ++ [first])
myCycle [] = []

akermann 0 n = n + 1
akermann m 0 = akermann (m -1) 1
akermann m n = akermann (m - 1) (akermann m (n - 1))

collatz 1 = 1
collatz n
  | even n = 1 + collatz (n `div` 2)
  | otherwise = 1 + collatz (n * 3 + 1)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' = fastFib 1 1

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c -1)
