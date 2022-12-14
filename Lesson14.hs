data SixSidedDice
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6
  deriving (Ord, Enum)

-- instance Show SixSidedDice where
--   show S1 = "one"
--   show S2 = "two"
--   show S3 = "three"
--   show S4 = "four"
--   show S5 = "five"
--   show S6 = "six"

instance Show SixSidedDice where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"

instance Eq SixSidedDice where
  (==) S1 S1 = True
  (==) S2 S2 = True
  (==) S3 S3 = True
  (==) S4 S4 = True
  (==) S5 S5 = True
  (==) S6 S6 = True
  (==) _ _ = False
  
-- newtype -> not lazy like data by limitation of one constructor and one type
newtype Name = Name (String, String) deriving (Eq, Show)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names =
  [ Name ("Emil", "Cioran"),
    Name ("Eugene", "Thacker"),
    Name ("Friedrich", "Nietzsche")
  ]
