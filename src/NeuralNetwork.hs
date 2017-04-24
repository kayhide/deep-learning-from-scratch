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

-- |
-- >>> meanSquaredError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.6, 0, 0.05, 0.1, 0, 0.1, 0, 0]
-- 9.750000000000003e-2
-- >>> meanSquaredError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.1, 0, 0.05, 0.1, 0, 0.6, 0.5, 0]
-- 0.7224999999999999
meanSquaredError :: [Double] -> [Double] -> Double
meanSquaredError t y = (/2) $ sum $ fmap (^2) $ zipWith (-) t y

-- |
-- >>> crossEntropyError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.6, 0, 0.05, 0.1, 0, 0.1, 0, 0]
-- 0.510825457099338
-- >>> crossEntropyError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.1, 0, 0.05, 0.1, 0, 0.6, 0.5, 0]
-- 2.302584092994546
crossEntropyError :: [Double] -> [Double] -> Double
crossEntropyError t y = negate $ sum $ zipWith (*) t $ fmap (log . (+delta)) y
  where delta = 1e-7 :: Double

-- |
-- >>> function1 x = 0.01 * x * x + 0.1 * x
-- >>> numericalDiff function1 5
-- 0.1999999999990898
-- >>> numericalDiff function1 10
-- 0.2999999999997449
-- >>> function2 x y = x * x + y * y
-- >>> numericalDiff (flip function2 4) 3
-- 6.00000000000378
-- >>> numericalDiff (function2 3) 4
-- 7.999999999999119
numericalDiff :: (Double -> Double) -> Double -> Double
numericalDiff f x = (f (x + h) - f (x - h)) / (2 * h)
  where h = 1e-4 :: Double
