{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module NeuralNetwork where

import Control.Lens
import Numeric.LinearAlgebra

stepFunction :: Double -> Double
stepFunction x | x > 0 = 1.0
               | otherwise = 0.0

sigmoidFunction :: Double -> Double
sigmoidFunction x = 1 / (1 + (exp (-x)))

reluFunction :: Double -> Double
reluFunction = max 0

-- |
-- >>> :set -XOverloadedLists
-- >>> softmax [1010, 1000, 990]
-- [0.999954600070331,4.539786860886666e-5,2.061060046209062e-9]
-- >>> softmax [0.3, 2.9, 4.0]
-- [1.821127329554753e-2,0.24519181293507392,0.7365969137693786]
-- >>> sumElements $ softmax [0.3, 2.9, 4.0]
-- 1.0
softmax :: Vector R -> Vector R
softmax xs = cmap (/denom) ys
  where ys = cmap calc xs
        calc x = exp (x - c)
        c = maxElement xs
        denom = sumElements ys

-- |
-- >>> :set -XOverloadedLists
-- >>> meanSquaredError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.6, 0, 0.05, 0.1, 0, 0.1, 0, 0]
-- 9.750000000000003e-2
-- >>> meanSquaredError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.1, 0, 0.05, 0.1, 0, 0.6, 0.5, 0]
-- 0.7224999999999999
meanSquaredError :: Vector R -> Vector R -> Double
meanSquaredError t y = (/2) $ sumElements $ cmap (^2) $ t - y

-- |
-- >>> :set -XOverloadedLists
-- >>> crossEntropyError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.6, 0, 0.05, 0.1, 0, 0.1, 0, 0]
-- 0.510825457099338
-- >>> crossEntropyError [0, 0, 1, 0, 0, 0, 0, 0, 0, 0] [0.1, 0.05, 0.1, 0, 0.05, 0.1, 0, 0.6, 0.5, 0]
-- 2.302584092994546
crossEntropyError :: Vector R -> Vector R -> Double
crossEntropyError t y = negate $ dot t $ cmap (log . (+delta)) y
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

-- |
-- >>> function2 v = dot v v
-- >>> numericalGradient function2 $ vector [3.0, 4.0]
-- [6.00000000000378,7.999999999999119]
-- >>> numericalGradient function2 $ vector [0.0, 2.0]
-- [0.0,4.000000000004]
-- >>> numericalGradient function2 $ vector [3.0, 0.0]
-- [6.000000000012662,0.0]
numericalGradient
  :: (Gradible c)
  => ((c R) -> R) -> c R -> c R
numericalGradient f v = assoc (size v) 0 xs
  where
    h = 1e-4 :: R
    size' = size v
    xs = do
      idx <- indicesOf v
      let v1 = v - assoc size' 0 [(idx, h)]
          v2 = v + assoc size' 0 [(idx, h)]
          x' = f v2 - f v1
      return (idx, x' / (2 * h))

-- |
-- >>> function2 v = dot v v
-- >>> gradientDescent function2 (vector [-3.0, 4.0]) 0.1 100
-- [-6.111107928998789e-10,8.148143905314271e-10]
gradientDescent :: ((Vector R) -> Double) -> Vector R -> Double -> Int -> Vector R
gradientDescent f v lr steps = gradientDescents f lr v !! steps

-- |
-- >>> function2 v = dot v v
-- >>> take 3 $ gradientDescents function2 0.1 (vector [-3.0, 4.0])
-- [[-3.0,4.0],[-2.399999999999622,3.200000000000088],[-1.9199999999982538,2.5599999999976717]]
gradientDescents :: ((Vector R) -> Double) -> Double -> Vector R -> [Vector R]
gradientDescents f lr v = iterate desc v
  where desc v = v - cmap (* lr) (numericalGradient f v)



class (Container c R, Num (c R)) => Gradible c where
  indicesOf :: c R -> [IndexOf c]

instance Gradible Vector where
  indicesOf v = take (size v) [0..]

instance Gradible Matrix where
  indicesOf w = (,) <$> (take (rows w) [0..]) <*> (take (cols w) [0..])
