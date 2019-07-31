module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "YES"
  else
    putStrLn "paw"
  where cool v =
          v == "penis"

