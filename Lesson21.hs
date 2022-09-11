import Distribution.TestSuite (TestInstance(name))
helloPerson :: String -> String
<<<<<<< HEAD
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let statement =   helloPerson name
=======
helloPerson name = "Hello" ++ " " ++ name ++ "!!"


main :: IO ()
main = do
  putStr  "Hello! What's your name? "
  name <- getLine
  let statement = helloPerson name
>>>>>>> d7166ed186a4488d463c849a59cbda103af381d2
  putStrLn statement
