module Ch4 where

import Control.Monad
import Numeric.LinearAlgebra
import Graphics.Gnuplot.Simple

import Perceptron
import NeuralNetwork
import qualified MNIST


main :: IO ()
main = do
  displayFunction1
  displayTangents
  displayFunction2
  displayGradients
  displayDescents

-- ch4_3_2
function1 :: Double -> Double
function1 x = 0.01 * x * x + 0.1 * x

displayFunction1 :: IO ()
displayFunction1 = plotPath attrs $ zip xs ys
  where attrs = [(Title "Function 1"), (YRange (0, 6))]
        xs = linearScale 100 (0, 20) :: [Double]
        ys = function1 <$> xs

tangent :: (Double -> Double) -> Double -> Double -> Double
tangent f x0 x = a * (x - x0) + f x0
  where a = numericalDiff f x0

displayTangents :: IO ()
displayTangents = plotPathsStyle attrs paths
  where attrs = [(Title "Tangents of Function 1"), (YRange (-1, 6))]
        xs = linearScale 100 (0, 20) :: [Double]
        paths = do
          (fn, name) <- [ (function1, "0.01 * x^2 + 0.1 * x")
                        , (tangent function1 5, "tangent (x = 5)")
                        , (tangent function1 10, "tangent (x = 10)")
                        ]
          let style = PlotStyle Lines $ CustomStyle [(LineTitle name)]
          return $ (style, zip xs (fn <$> xs))

-- ch4_3_3
function2 :: Vector R -> Double
function2 v = dot v v

displayFunction2 :: IO ()
displayFunction2 = plotFunc3d attrs [] xs ys function2'
  where
    attrs = [Title "Function 2"]
    xs = linearScale 100 (-3, 3) :: [Double]
    ys = linearScale 100 (-3, 3) :: [Double]
    function2' x y = function2 $ vector [x, y]

displayGradients :: IO ()
displayGradients = plotListStyle attrs style vs
  where
    attrs = [Title "Gradients of Function 2"]
    style = PlotStyle Vectors $ CustomStyle [(LineTitle "gradients")]
    xs = [(-2.0), (-1.75) .. 2.0] :: [Double]
    ys = [(-2.0), (-1.75) .. 2.0] :: [Double]
    vs = do
      x <- xs
      y <- ys
      let [dx, dy] = toList $ numericalGradient function2 $ vector [x, y]
      return ((x, y), (- dx / 20, - dy / 20))

displayDescents :: IO ()
displayDescents = plotPathStyle attrs style vs
  where
    attrs = [Title "Gradient descents of Function 2", XRange (-4, 4), YRange (-4, 4)]
    style = PlotStyle Points $ CustomStyle [(LineTitle "descents")]
    v0 = vector [-3.0, 4.0]
    vs = do
      v <- take 100 $ gradientDescents function2 0.1 v0
      let [x, y] = toList v
      return (x, y)
