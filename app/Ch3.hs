{-# LANGUAGE FlexibleContexts #-}
module Ch3 where

import Numeric.LinearAlgebra
import NeuralNetwork

type ActivationFunction = Double -> Double
type Layer = (Matrix R, Vector R, ActivationFunction)
type Network = [Layer]

network :: Network
network = [ ( (2><3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
            , vector [0.1, 0.2, 0.3]
            , sigmoidFunction
            )
          , ( (3><2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
            , vector [0.1, 0.2]
            , sigmoidFunction
            )
          , ( (2><2) [0.1, 0.3, 0.2, 0.4]
            , vector [0.1, 0.2]
            , id
            )
          ]

forward :: Network -> Vector R -> Vector R
forward nw x = foldl propagate x nw
  where propagate x (w, b, f) = cmap f $ x <# w + b


x = vector [1.0, 0.5]
y = forward network x
