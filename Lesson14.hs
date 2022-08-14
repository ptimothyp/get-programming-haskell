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
