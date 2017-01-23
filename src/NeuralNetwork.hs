module NeuralNetwork where

stepFunction :: Double -> Double
stepFunction x | x > 0 = 1.0
               | otherwise = 0.0
