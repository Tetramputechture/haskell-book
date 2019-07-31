module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter a name: "
  name <- getLine
  putStrLn "Please enter an age above 0: "
  ageStr <- getLine
  let age = read ageStr
  case mkPerson name age of
    (Right p)       -> putStrLn $ "Yay! Person: " ++ (show p)
    (Left invalid)  -> putStrLn $ "Error: " ++ (show invalid)
