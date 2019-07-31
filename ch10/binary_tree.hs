module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
            => a
            -> BinaryTree a
            -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder (Node Leaf a Leaf) = [a]
preorder (Node left a right) =
  [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder (Node Leaf a Leaf) = [a]
inorder (Node left a right) =
  (inorder left) ++ [a] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder (Node Leaf a Leaf) = [a]
postorder (Node left a right) =
  (postorder right) ++ [a] ++ (postorder left)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "NO"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "NO"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [3, 2, 1]
  then putStrLn "Postorder fine!"
  else putStrLn "NO"

foldTree :: (a -> b -> b)
          -> b
          -> BinaryTree a
          -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = foldTree f base' left
  where
  base' = fn a base''
  base'' = foldTree fn base right
