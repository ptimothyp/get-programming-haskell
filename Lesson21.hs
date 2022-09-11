import Distribution.TestSuite (TestInstance(name))
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!!"


main :: IO ()
main = do
  putStr  "Hello! What's your name? "
  name <- getLine
  let statement = helloPerson name
  putStrLn statement
