import qualified Data.Map as Map

newtype Box a = Box a deriving (Show)

wrap x = Box x

unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 2.9 3.0 4.0

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Philip" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

data List a = Empty | Cons a (List a) deriving (Show)

builtInEx1 :: [Int]
builtInEx1 = 1 : 2 : 3 : []

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap f Empty = Empty
ourMap f (Cons x rest) = Cons (f x) (ourMap f rest)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog = Map.fromList organPairs
