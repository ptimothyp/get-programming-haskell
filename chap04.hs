addressLetter name location = nameText ++ " - " ++ location
  where
    nameText = fst name ++ " " ++ snd name

me = ("Tim", "Pakkianathan")

location = "Gininderry"

-- addressLetter me location

addressLetter' name location = locationFunction name
  where
    locationFunction = getLocationFunction location

sfOffice name =
  if lastName < "L"
    then
      nameText
        ++ "- PO Box 1234 - San Francisco, CA, 941111"
    else
      nameText
        ++ "- PO Box 1010 - San Francisco, CA, 94109"
  where
    (firstName, lastName) = name
    nameText = firstName ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    (firstName, lastName) = name
    nameText = firstName ++ " " ++ lastName

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> fst name ++ " " ++ snd name)
