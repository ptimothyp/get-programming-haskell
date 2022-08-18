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
largestAlphabetNumber = fromEnum (maxBound :: FourLetterAlphabet)

-- rotChar :: Char -> Char
-- rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
--   where sizeOfAlphabet = largestCharNumber + 1

message :: [FourLetterAlphabet]
message = [L1, L3, L3, L1, L1, L1]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot4l
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize

data ThreeLetterAlphabet
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot3l
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3l = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else 1 + fromEnum c + halfN
    rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map rot3ldecoder
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
    rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder = map rotChar
  where
    sizeOfAlphabet = fromEnum (maxBound :: Char) + 1
    rotChar = rotN sizeOfAlphabet

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
  where
    sizeOfAlphabet = fromEnum (maxBound :: Char) + 1
    rotCharDecoder = rotNdecoder sizeOfAlphabet

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && not (value1 && value2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor = zipWith (curry xorPair)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - length reversedBits
    leadingFalses = replicate missingBits False

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
  where
    size = length bits
    indices = [size -1, size -2 .. 0]
    trueLocations =
      filter
        fst
        (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =
  map
    (\pair -> (fst pair) `xor` (snd pair))
    (zip padBits plaintextBits)
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    bitList = applyOTP' pad plaintext

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])
