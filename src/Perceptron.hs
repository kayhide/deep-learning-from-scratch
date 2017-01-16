module Perceptron where

type Gate = Float -> Float -> Float

makeGate :: Float -> Float -> Float -> Gate
makeGate w1 w2 b x1 x2
  | x1 * w1 + x2 * w2 + b <= 0 = 0
  | otherwise = 1

-- |
-- >>> andGate 0 0
-- 0.0
-- >>> andGate 0 1
-- 0.0
-- >>> andGate 1 0
-- 0.0
-- >>> andGate 1 1
-- 1.0
andGate :: Gate
andGate = makeGate 0.5 0.5 (negate 0.7)

-- |
-- >>> nandGate 0 0
-- 1.0
-- >>> nandGate 0 1
-- 1.0
-- >>> nandGate 1 0
-- 1.0
-- >>> nandGate 1 1
-- 0.0
nandGate :: Gate
nandGate = makeGate (negate 0.5) (negate 0.5) 0.7

-- |
-- >>> orGate 0 0
-- 0.0
-- >>> orGate 0 1
-- 1.0
-- >>> orGate 1 0
-- 1.0
-- >>> orGate 1 1
-- 1.0
orGate :: Gate
orGate = makeGate 0.5 0.5 (negate 0.2)

-- |
-- >>> xorGate 0 0
-- 0.0
-- >>> xorGate 0 1
-- 1.0
-- >>> xorGate 1 0
-- 1.0
-- >>> xorGate 1 1
-- 0.0
xorGate :: Gate
xorGate x1 x2 = andGate (nandGate x1 x2) (orGate x1 x2)
