type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int
type Weight = Int
type PatientName = (String, String)

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo firstName lastName age height = name ++ ageHeight
  where
  name = lastName ++ ", " ++ firstName
  ageHeight = " (" ++ show age ++ "yrs. " ++ show height ++ "in.)"

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' name age height = named ++ ageHeight
  where
  named = lastName name ++ ", " ++ firstName name
  ageHeight = " (" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

instance Show Sex where
  show Male = "M"
  show Female = "F"

data AboType = A | B | AB | O
data RhType = Pos | Neg
data BloodType = BloodType AboType RhType

instance Show AboType where
  show A = "A"
  show B = "B"
  show AB = "AB"
  show O = "O"

instance Show RhType where
  show Pos = "+"
  show Neg = "-"

instance Show BloodType where
  show (BloodType abo rh) = show abo ++ show rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

data Name = Name FirstName LastName
          | NameWithMiddle FirstName LastName MiddleName

instance Show Name
  where
    show (Name firstName lastName) = lastName ++ ", " ++  firstName
    show (NameWithMiddle firstName middleName lastName) = lastName ++ ", " ++ middleName ++ " - " ++ firstName


me = Name "Tim" "Pakkianathan"

meWithM = NameWithMiddle "Tim" "Sujit" "Pakkianathan"

data Patient = Patient
  { name :: Name
  , sex :: Sex
  , age :: Age
  , height :: Height
  , weight :: Weight
  , bloodType :: BloodType
  } deriving Show

johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                       , age = 43
                       , sex = Female
                       , height = 62
                       , weight = 115
                       , bloodType = BloodType O Neg }
