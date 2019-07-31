module Unfolds where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

myIterate :: (a -> a) -> a -> [a]
myIterate fn start = [start] ++ (myIterate fn $ fn start)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr fn start =
  case result of
    (Just (a, b)) -> [a] ++ (myUnfoldr fn b)
    _        -> []
  where result = fn start

betterIterate :: (a -> a) -> a -> [a]
betterIterate fn = myUnfoldr (\x -> Just (x, fn x))

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold fn start =
  case result of
    (Just (x, y, z)) -> Node (unfold fn x) y (unfold fn z)
    _                -> Leaf
  where result = fn start

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeBuilder 0
  where
    treeBuilder a
      | a < n = Just (a + 1, a, a + 1)
      | otherwise = Nothing
