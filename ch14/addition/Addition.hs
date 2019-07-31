module Addition where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import WordNumber
  (digitToWord, digits, wordNumber)

import Hangman
import Cipher

half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == (halfIdentity x)

runQc1 :: IO ()
runQc1 = quickCheck prop_halfIdentity

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered = listOrdered . sort

runQc2 :: IO ()
runQc2 = quickCheck prop_listOrdered

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

runQc3 :: IO ()
runQc3 = quickCheck plusAssociative

runQc4 :: IO ()
runQc4 = quickCheck plusAssociative

multiplyAssociative :: Int -> Int -> Int -> Bool
multiplyAssociative x y z =
  x * (y * z) == (x * y) * z

multiplyCommutative :: Int -> Int -> Bool
multiplyCommutative x y =
  x * y == y * x

runQc5 :: IO ()
runQc5 = quickCheck multiplyAssociative

runQc6 :: IO ()
runQc6 = quickCheck multiplyCommutative

quotRem' :: Int -> Int -> Bool
quotRem' x y =
  (quot x y)*y + (rem x y) == x

divRem' :: Int -> Int -> Bool
divRem' x y =
  (div x y)*y + (mod x y) == x

runQc7 :: IO ()
runQc7 = quickCheck quotRem'

runQc8 :: IO ()
runQc8 = quickCheck divRem'

expAssociative :: Int -> Int -> Int -> Bool
expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

expCommutative :: Int -> Int -> Bool
expCommutative x y =
  x ^ y == y ^ x

runQc9 :: IO ()
runQc9 = quickCheck expAssociative

runQc10 :: IO ()
runQc10 = quickCheck expCommutative

twiceReverse :: [Int] -> Bool
twiceReverse xs = (reverse $ reverse xs) == (id xs)

runQc11 :: IO ()
runQc11 = quickCheck twiceReverse

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001
        `shouldBe` "nine-zero-zero-one"

samplePuzzle = freshPuzzle "Test"
samplePuzzle' = Puzzle "Test" [] ['s']

testHangman :: IO ()
testHangman = hspec $ do
  describe "fillInCharacter" $ do
    it "populates guessed letter" $ do
      (fillInCharacter samplePuzzle 'c') 
        `shouldBe` (Puzzle "Test" [Nothing, Nothing, Nothing, Nothing] ['c'])
    it "populates correct letter" $ do
      (fillInCharacter samplePuzzle 's')
        `shouldBe` (Puzzle "Test" [Nothing, Nothing, Just 's', Nothing] ['s'])
    
  describe "handleGuess" $ do
    it "handles already guessed" $ do
      (handleGuess samplePuzzle' 's')
      `shouldReturn` (Puzzle "Test" [] ['s'])
    it "handles correct guess" $ do
      (handleGuess samplePuzzle 's')
      `shouldReturn` (Puzzle "Test" [Nothing, Nothing, Just 's', Nothing] ['s'])
    it "handles incorrect guess" $ do
      (handleGuess samplePuzzle 'q')
      `shouldReturn` (Puzzle "Test" [Nothing, Nothing, Nothing, Nothing] ['q'])

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

genSafeInt :: Gen Int
genSafeInt = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> x > 0 && x < 27)

prop_rCipher :: Property
prop_rCipher = forAll genSafeInt $ \n ->
               forAll genSafeString $ \str -> str == rightCaesar (26 - n) (rightCaesar n str)

testCipher :: IO ()
testCipher = do
  quickCheck prop_rCipher
