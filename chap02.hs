simple :: p -> p
simple x = x

helloworld = "hello world"
x = 2

calcChange owed given = if change > 0 then change else 0
    where change = given - owed

calcChange' owed given = let change = given - owed in
                          if change > 0 then change else 0

counter x = let x = x + 1 in
               x

counter' x = (\y -> x + y) x

ifEven doThis x = if even x
                     then doThis x
                     else
                      x

inc x = x + 1
double x = 2 * x
square x = x ^ 2

ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

ifEvenCube = ifEven (^ 3)

author = ("William", "Shakespeare")
getFirst (fst, rest) = fst
getSecond (_, second, rest) = second


names = [("Ian", "Curtis"),
          ("Bernard","Sumner"),
          ("Peter", "Hook"),
          ("Stephen","Morris")]

compareLastNames name1 name2
    | lastName1 > lastName2 = GT
    | lastName1 < lastName2 = LT
    | firstName1 > firstName2 = GT
    | firstName1 < firstName2 = LT
    | otherwise = EQ
    where
      (firstName1, lastName1) = name1
      (firstName2, lastName2) = name2

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

test_LastNames = compareLastNames ("Ian", "Curtis") ("Bernard","Sumner")

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

instance Ord Person where
  compare = comparePerson

father = Person "Timothy" "Pakkianathan"
son = Person "Nathaniel" "Pakkianathan"

test_comparePerson = comparePerson father son

-- Usage
-- sortBy compareLastNames names

