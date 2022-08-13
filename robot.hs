-- robot (name,attack, hp) = \message -> message (name, attack, hp)
robot' (name, attack, hp) message = message (name, attack, hp)

name (name, _, _) = name
attack (_, attack, _) = attack
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

printRobot aRobot = aRobot (\(name, attack, hp) -> name ++ " attack:" ++ show attack ++ " hp:"++ show hp)
damage aRobot attackDamage = aRobot (\(name, attack, hp) -> robot' (name, attack, hp-attackDamage))

fight attacker defender = damage defender attack
  where attack = if getHp attacker > 10
                    then getAttack attacker
                    else 0

data Robot = Robot { name:: String
                   ,attack ::Integer
                   , hp:: Integer
                   }
instance Show Robot where
  show (Robot name attack hp) = name ++ " attack:" ++ show attack ++ " hp:"++ show hp

myRobot = robot' ("Hero", 5, 400)
