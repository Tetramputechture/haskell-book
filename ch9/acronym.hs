module Acronym where

makeAcronym str = [x | x <- str,  elem x ['A'..'Z']]
