module Person where

type FirstName = String
type LastName = String

data Person = Person
  {
    firstName :: FirstName
  , lastName :: LastName
  }

instance Show Person where
  show (Person firstName lastName) = lastName ++ ", " ++ firstName

instance Eq Person where
  Person firstName1 lastName1 == Person firstName2 lastName2 = firstName1 == firstName2 && lastName1 == lastName2

comparePerson:: Person -> Person -> Ordering
comparePerson name1 name2
    | lastName1 > lastName2 = GT
    | lastName1 < lastName2 = LT
    | firstName1 > firstName2 = GT
    | firstName1 < firstName2 = LT
    | otherwise = EQ
    where
      Person firstName1 lastName1 =  name1
      Person firstName2 lastName2 =  name2

instance Ord Person where
  compare = comparePerson

father = Person "Timothy" "Pakkianathan"
son = Person "Nathaniel" "Pakkianathan"

test_comparePerson = comparePerson father son

