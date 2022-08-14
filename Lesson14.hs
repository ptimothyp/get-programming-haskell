data SixSidedDice = S1
                  | S2
                  | S3
                  | S4
                  | S5
                  | S6

-- instance Show SixSidedDice where
--   show S1 = "one"
--   show S2 = "two"
--   show S3 = "three"
--   show S4 = "four"
--   show S5 = "five"
--   show S6 = "six"

instance Show SixSidedDice where
  show S1 = "1"
  show S2 = "2"
  show S3 = "3"
  show S4 = "4"
  show S5 = "5"
  show S6 = "6"

instance Eq SixSidedDice where
  (==) S1 S1 = True
  (==) S2 S2 = True
  (==) S3 S3 = True
  (==) S4 S4 = True
  (==) S5 S5 = True
  (==) S6 S6 = True
  (==) _ _ = False
