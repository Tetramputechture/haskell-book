module MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ fn (Just x) = fn x
mayybee nothing _ Nothing = nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe nothing = mayybee nothing id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x] 

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . fmap maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | length values == length xs = Just values
  | otherwise = Nothing
  where values = catMaybes xs
