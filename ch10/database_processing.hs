import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

dateFolder :: DatabaseItem -> [UTCTime] -> [UTCTime]
dateFolder (DbDate x) xs = x:xs
dateFolder _ xs = xs

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate = foldr dateFolder []

numberFolder :: DatabaseItem -> [Integer] -> [Integer]
numberFolder (DbNumber x) xs = x:xs
numberFolder _ xs = xs

filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldr numberFolder []

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = foldr (\x y -> max x y) dayone . filterDbDate
            where dayone = UTCTime (fromGregorian 0 0 0)
                                  (secondsToDiffTime 0)

sumDb :: [DatabaseItem]
      -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem]
      -> Double
avgDb db = (fromIntegral (sum nums)) / (fromIntegral (length nums))
  where nums = filterDbNumber db
