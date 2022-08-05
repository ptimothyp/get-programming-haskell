import Chap04

ifEven f x =
  if even x
    then f x
    else x

inc x = 1 + x

double x = 2 * x

square x = x ^ 2

cube x = x ^ 3

ifEvenInc = ifEven inc

genIfXEven x = (`ifEven` x)

getRequestUrl host apiKey resource id =
  host
    ++ "/"
    ++ resource
    ++ "/"
    ++ id
    ++ "/"
    ++ apiKey

getBookFromExample = getRequestUrl "http://example.com" "1337hAsk3ll" "book"

flipBinaryArgs func arg1 arg2 = func arg2 arg1

flipBinaryArgs' func = \x y -> func y x

addressLetterFlip = flipBinaryArgs addressLetter

adressLetterNY = addressLetterFlip "NY"
