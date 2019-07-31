chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = fn x == y

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith fnYtoX x y = fromInteger (fnYtoX y) + x

fn :: Num a => a -> a
fn x = x
