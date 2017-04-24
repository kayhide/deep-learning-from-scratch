module Ch4 where

import Control.Monad
import Graphics.Gnuplot.Simple

import Perceptron
import NeuralNetwork
import qualified MNIST


-- ch4_3_2
function1 :: Double -> Double
function1 x = 0.01 * x * x + 0.1 * x

displayFunction1 :: IO ()
displayFunction1 = plotPath attrs $ zip xs ys
  where attrs = [(Title "function 1"), (YRange (0, 6))]
        xs = linearScale 100 (0, 20) :: [Double]
        ys = function1 <$> xs

tangent :: (Double -> Double) -> Double -> Double -> Double
tangent f x0 x = a * (x - x0) + f x0
  where a = numericalDiff f x0

displayTangents :: IO ()
displayTangents = plotPathsStyle attrs paths
  where attrs = [(YRange (-1, 6))]
        xs = linearScale 100 (0, 20) :: [Double]
        paths = do
          (fn, name) <- [ (function1, "0.01 * x^2 + 0.1 * x")
                        , (tangent function1 5, "tangent (x = 5)")
                        , (tangent function1 10, "tangent (x = 10)")
                        ]
          let style = PlotStyle Lines $ CustomStyle [(LineTitle name)]
          return $ (style, zip xs (fn <$> xs))

-- ch4_3_3
function2 :: Double -> Double -> Double
function2 x0 x1 = x0 * x0 + x1 * x1

displayFunction2 :: IO ()
displayFunction2 = plotMesh3d [] [] $ do
  x <- xs
  return $ do
    y <- xs
    return (x, y, function2 x y)
      where xs = linearScale 100 (-3, 3) :: [Double]
