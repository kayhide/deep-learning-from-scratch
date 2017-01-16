module Main where

import Control.Monad
import Perceptron

runAndPrint op x1 x2 = do
  putStr $ show (x1, x2)
  putStr " "
  print $ op x1 x2

main :: IO ()
main = do
  putStrLn "AND"
  sequence $ runAndPrint andGate <$> [0, 1] <*> [0, 1]
  putStrLn "NAND"
  sequence $ runAndPrint nandGate <$> [0, 1] <*> [0, 1]
  putStrLn "OR"
  sequence $ runAndPrint orGate <$> [0, 1] <*> [0, 1]
  putStrLn "XOR"
  sequence $ runAndPrint xorGate <$> [0, 1] <*> [0, 1]
  return ()
