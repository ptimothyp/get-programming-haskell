cup flOz = \message -> message flOz
cup' flOz message = message flOz

cup6 = cup 6
cup6' = cup' 6
cup12 = cup 12

getOz cup = cup (\flOz -> flOz)
getz' cup = cup id

drink aCup ozDrank
  | ozLeft < 0 = cup 0
  | otherwise = cup ozLeft
  where
    flOz = getz' aCup
    ozLeft = flOz - ozDrank

drink' aCup ozDrank = if ozDiff >= 0
                      then cup ozDiff
                      else cup 0
   where flOz = getOz aCup
         ozDiff = flOz - ozDrank

afterBigGulp = drink' cup6 10

afterSomeDrinks = getOz (foldl drink cup6 [1,4,1])
afterFewDrinks = getOz (foldr (flip drink) cup6 [1,4,1])

