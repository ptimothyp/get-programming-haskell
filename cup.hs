cup f10z = \message -> message f10z
cup' f10z message = message f10z

cup6 = cup 10
cup6' = cup' 12
cup12 = cup 14

getz cup = cup (\f10z -> f10z)


