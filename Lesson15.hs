data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

-- largestCharNumber :: Int
-- largestCharNumber = fromEnum (maxBound:: Char)

largestAlphabetNumber :: Int
largestAlphabetNumber = fromEnum(maxBound:: FourLetterAlphabet)

-- rotChar :: Char -> Char
-- rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
--   where sizeOfAlphabet = largestCharNumber + 1

message :: [FourLetterAlphabet]
message = [L1, L3, L3, L1, L1, L1]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where 
    alphaSize = 1 + fromEnum(maxBound:: FourLetterAlphabet)
    rot4l = rotN alphaSize
    
data ThreeLetterAlphabet = Alpha
                         | Beta
                         | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where 
    alphaSize = 1 + fromEnum(maxBound:: ThreeLetterAlphabet)
    rot3l = rotN alphaSize


rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                    then fromEnum c + halfN
                    else 1 + fromEnum c + halfN
        rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
  where 
    alphaSize = 1 + fromEnum (maxBound:: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize


rotEncoder :: String -> String
rotEncoder vals = map rotChar vals
  where
    sizeOfAlphabet = fromEnum (maxBound:: Char) + 1
    rotChar = rotN sizeOfAlphabet

rotDecoder :: String -> String
rotDecoder vals = map rotCharDecoder vals
  where
    sizeOfAlphabet = fromEnum (maxBound:: Char) + 1
    rotCharDecoder = rotNdecoder sizeOfAlphabet
