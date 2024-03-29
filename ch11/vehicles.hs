module Vehicles where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | Catapults | Chances deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Integer deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 5

isCar :: Vehicle -> Bool
isCar x =
  case x of
    Car _ _ -> True
    _ -> False

isPlane :: Vehicle -> Bool
isPlane x =
  case x of 
    Plane _ _ -> True
    _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu x = 
  case x of
    Car manu _ -> manu
    _ -> undefined

getSize :: Vehicle -> Integer
getSize x =
  case x of
    Plane _ size -> size
    _ -> undefined
