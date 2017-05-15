module SimpleNet where

import System.Random
import Numeric.LinearAlgebra

import NeuralNetwork

newtype SimpleNet = SimpleNet { unSimpleNet :: Matrix R }
  deriving (Eq, Show)

makeRandomNet :: Int -> Int -> IO SimpleNet
makeRandomNet rows cols = SimpleNet <$> rand rows cols

-- |
-- >>> net = SimpleNet $ (2 >< 3) [0.47355232, 0.99773930, 0.84668094, 0.85557411, 0.03563661, 0.69422093]
-- >>> x = vector [0.6, 0.9]
-- >>> predict net x
-- [1.054148091,0.630716529,1.132807401]
predict :: SimpleNet -> Vector R -> Vector R
predict (SimpleNet w) x = x <# w

-- |
-- >>> net = SimpleNet $ (2 >< 3) [0.47355232, 0.99773930, 0.84668094, 0.85557411, 0.03563661, 0.69422093]
-- >>> x = vector [0.6, 0.9]
-- >>> t = vector [0, 0, 1]
-- >>> loss net x t
-- 9.746346258628767
loss :: SimpleNet -> Vector R -> Vector R -> Double
loss net@(SimpleNet w) x t = crossEntropyError y t
  where
    z = predict net x
    y = softmax z
