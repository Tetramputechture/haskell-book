module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "Whats shakin?"
  else
    putStrLn "psh"
  where cool =
          coolness == "fuckin shit"

