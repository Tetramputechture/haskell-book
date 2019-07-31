module Phone where

import Data.Char

type Digit = Char
type PhoneValues = String

data DaPhone = DaPhone [(Digit, PhoneValues)] deriving (Eq, Show)

daphone = DaPhone
  [('1', "1"),
   ('2', "abc"),
   ('3', "def"),
   ('4', "ghi"),
   ('5', "jkl"),
   ('6', "mno"),
   ('7', "pqrs"),
   ('8', "tuv"),
   ('9', "wxyz"),
   ('*', "^"),
   ('0', "+ _"),
   ('#', ".,")]

convo :: [String]
convo = 
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

type Presses = Int

getShiftChar :: DaPhone 
             -> Digit
getShiftChar (DaPhone lst) = 
  fst ((filter (\x -> snd x == "^") lst) !! 0)

reverseTaps :: DaPhone 
            -> Char
            -> [(Digit, Presses)]
reverseTaps phone c 
  | isUpper c = [(getShiftChar phone, 1)] ++ reverseTaps' phone (toLower c)
  | otherwise = reverseTaps' phone c

reverseTaps' :: DaPhone
             -> Char
             -> [(Digit, Presses)]
reverseTaps' phone c = 
  [numPresses tup c]
  where tup = getPhoneTup phone c

getPhoneTup :: DaPhone
            -> Char
            -> (Digit, PhoneValues)
getPhoneTup (DaPhone lst) c =
  head (filter (\x -> elem c (snd x) || c == (fst x)) lst)

numPresses :: (Digit, PhoneValues)
           -> Char
           -> (Digit, Presses)
numPresses (d, t@(x:xs)) c
  | c == x = (d, 1)
  | c == d = (d, length t + 1)
  | elem c t == False = (d, 0)
  | otherwise = (d, 1 + snd (numPresses (d, xs) c))
  
cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)  

convertedConvo :: [(Digit, Presses)]
convertedConvo = concat $ map (cellPhonesDead daphone) convo

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

