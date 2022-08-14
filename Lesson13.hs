class Describable a where
  describe :: a -> String

data IceCream = Chocolate | Vanilla

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2
