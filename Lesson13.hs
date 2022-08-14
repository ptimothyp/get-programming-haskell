class Describable a where
  describe :: a -> String

data IceCream = Chocolate | Vanilla deriving (Show, Ord, Eq)
