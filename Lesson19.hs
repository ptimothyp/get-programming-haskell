import Data.List
import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen | Lung deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog = Map.fromList organPairs

data MyMaybe a = None | Something a

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- hasOrgan :: Organ -> Bool
hasOrgan organ = any (\x -> x == Just organ)

-- countOrgan :: Organ -> Int

countOrgan organ available = length (filter (\x -> x == Just organ) available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan Nothing = ""
showOrgan (Just organ) = show organ

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitcher | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitcher, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) =
  show container
    ++ " in the "
    ++ show location

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where
    organ = Map.lookup id catalog
