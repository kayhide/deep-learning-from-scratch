module NeuralNetwork where

stepFunction :: Double -> Double
stepFunction x | x > 0 = 1.0
               | otherwise = 0.0

sigmoidFunction :: Double -> Double
sigmoidFunction x = 1 / (1 + (exp (-x)))

reluFunction :: Double -> Double
reluFunction = max 0

-- |
-- >>> softmax [1010, 1000, 990]
-- [0.999954600070331,4.539786860886666e-5,2.061060046209062e-9]
-- >>> softmax [0.3, 2.9, 4.0]
-- [1.821127329554753e-2,0.24519181293507392,0.7365969137693786]
-- >>> sum $ softmax [0.3, 2.9, 4.0]
-- 1.0
softmax :: [Double] -> [Double]
softmax xs = map (/denom) ys
  where ys = map calc xs
        calc x = exp (x - c)
        c = maximum xs
        denom = sum ys
