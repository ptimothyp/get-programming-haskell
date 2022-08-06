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

subtract2 =  subtract 2
subtract2' = (flip (-)) 2

binaryPartionApplication binaryFunc x = \y -> binaryFunc x y

-- subseq::Integral a => a -> a -> [b] -> [b]
subseq start end list = reverse (drop fromend (reverse (drop start list)))
  where fromend = length list - end

-- myrepeat:: a -> [a]
myrepeat a = a:myrepeat a

-- inFirstHalf :: (Foldable t, Eq a) => a -> t a -> Bool
inFirstHalf :: (Eq a) => a -> [a] -> Bool

inFirstHalf x xs = x `elem` firstHalf
  where
    half = length xs `div` 2
    firstHalf = take half xs

-- inFirstHalf' :: (Foldable t, Eq a) => a -> t a -> Bool
-- inFirstHalf' x [] = False
-- inFirstHalf' x xs =

