
data Robot = Robot { name:: String
                   ,attack ::Int
                   , hp:: Int
                   }
class (Show a) => RoboActions a where
  damage:: a -> Int -> Robot

instance Show Robot where
  show (Robot name attack hp) = name ++ " attack:" ++ show attack ++ " hp:"++ show hp

robot1 = Robot "TRex" 50 1000
robot2 = Robot {name="Tauren", attack=60, hp=2000}
