{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Ch3 where

import Text.Printf
import Numeric.LinearAlgebra
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml

import qualified MNIST as MNIST
import NeuralNetwork

type ActivationFunction = [Double] -> [Double]
type Layer = (Matrix R, Vector R, ActivationFunction)
type Network = [Layer]

-- ch3_4
network :: Network
network = [ ( (2><3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
            , vector [0.1, 0.2, 0.3]
            , map sigmoidFunction
            )
          , ( (3><2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
            , vector [0.1, 0.2]
            , map sigmoidFunction
            )
          , ( (2><2) [0.1, 0.3, 0.2, 0.4]
            , vector [0.1, 0.2]
            , id
            )
          ]

forward :: Network -> Vector R -> Vector R
forward nw x = foldl propagate x nw
  where propagate x (w, b, f) = fromList . f . toList $ x <# w + b

x = vector [1.0, 0.5]
y = forward network x


-- ch3_6
readMatrix :: FilePath -> IO (Matrix R)
readMatrix file = do
  s <- BS.readFile file
  let Just m = Yaml.decode s :: Maybe [[Double]]
  return $ fromLists m

readVector :: FilePath -> IO (Vector R)
readVector file = do
  s <- BS.readFile file
  let Just v = Yaml.decode s :: Maybe [Double]
  return $ fromList v

readNetwork :: IO Network
readNetwork = do
  w1 <- readMatrix' "W1"
  b1 <- readVector' "b1"
  w2 <- readMatrix' "W2"
  b2 <- readVector' "b2"
  w3 <- readMatrix' "W3"
  b3 <- readVector' "b3"
  return [ (w1, b1, map sigmoidFunction)
         , (w2, b2, map sigmoidFunction)
         , (w3, b3, (toList . softmax . vector))
         ]
  where readMatrix' f = readMatrix $ "data/mnist/" ++ f ++ ".yml"
        readVector' f = readVector $ "data/mnist/" ++ f ++ ".yml"

predict :: Network -> [Double] -> [Double]
predict network = toList . (forward network) . vector

displayPrediction :: [Double] -> IO ()
displayPrediction xs = do
  mapM_ print' $ zip xs [0..]
  where print' (x, i) = do
          printf "%d %5.2f%% " (i :: Int) (x * 100)
          putStrLn $ replicate (floor (x * 20)) '*'

readImages :: IO [MNIST.Image]
readImages = do
  (MNIST.Images _ _ _ imgs) <- MNIST.getImages MNIST.Test
  return imgs

run :: [MNIST.Image] -> IO ()
run imgs = do
  predictor <- predict <$> readNetwork
  mapM_ (predict' predictor) imgs
  where predict' predictor img = do
          MNIST.displayImage img
          displayPrediction $ predictor $ concat img

main :: IO ()
main = run =<< readImages
