{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, a) = n > 42 && a == "Goats"

instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 42

