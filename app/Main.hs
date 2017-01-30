{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad
import Graphics.Gnuplot.Simple
import Numeric.LinearAlgebra
import Perceptron
import NeuralNetwork

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

displayActivationFunction :: IO ()
displayActivationFunction = plotPathsStyle attrs paths
  where attrs = [(Title "activation functions"), (YRange (-0.1, 5.0))]
        xs = linearScale 100 (-5.0, 5.0)
        paths = do
          (fn, name) <- [ (stepFunction, "step function")
                        , (sigmoidFunction, "sigmoid function")
                        , (reluFunction, "ReLU function")
                        ]
          let style = PlotStyle Lines $ CustomStyle [(LineTitle name)]
          return $ (style, zip xs (fn <$> xs))


-- | Test for drawing chart
displaySin :: IO ()
displaySin = plotPath [(Title "hello")] points
  where points :: [(Double, Double)]
        points = zip xs ys
        xs = linearScale 100 (0.0, 6.0)
        ys = sin <$> xs

-- | Test for drawing chart with multi paths
displaySinCos :: IO ()
displaySinCos = plotPathsStyle attrs [(style1, zip xs ys1), (style2, zip xs ys2)]
  where attrs = [(Title "sin and cos"), (XLabel "x"), (YLabel "y)]")]
        xs = linearScale 100 (0.0, 6.0) :: [Double]
        ys1 = sin <$> xs
        ys2 = cos <$> xs
        style1 = PlotStyle Lines $ CustomStyle [(LineTitle "sin")]
        style2 = PlotStyle Lines $ CustomStyle [(LineTitle "cos")]


-- | ch3 - 4
type Network = [(Matrix R, Vector R)]
network :: Network
network = [ ( (3><2) [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
            , vector [0.1, 0.2, 0.3]
            )
          , ( (2><2) [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
            , vector [0.1, 0.2]
            )
          , ( (2><2) [0.1, 0.2, 0.3, 0.4]
            , vector [0.1, 0.2]
            )
          ]

forward :: Network -> Vector R -> Vector R
forward nw x = foldl (activate . weigh) x nw
  where weigh x (w, b) = w #> x + b
        activate = cmap sigmoidFunction


x = vector [1.0, 0.5]
y = cmap id a3
