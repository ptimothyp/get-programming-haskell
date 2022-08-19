myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myMM f = (foldr (||) False) . (map f)

myM f xs = map f xs

data Color
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | None
  deriving (Show, Eq)

-- instance Semigroup Color where
--   (<>) Red Blue = Purple
--   (<>) Blue Red = Purple
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Yellow Red = Orange
--   (<>) Red Yellow = Orange
--   (<>) a b =
--     if a == b
--       then a
--       else Brown

instance Semigroup Color where
  (<>) None None = None
  (<>) a None = a
  (<>) None b = b
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

instance Monoid Color where
  mempty = None
  mappend = (<>)

type Events = [String]

type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents = cartCombine combiner
  where
    combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tail"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]