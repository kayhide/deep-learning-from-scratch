{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module MNIST where

import Codec.Compression.GZip (decompress)
import Control.Monad (when)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Network.HTTP.Simple (Request, httpLBS, getResponseBody)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.List.Extra as List
import System.FilePath
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Proxy
import Data.Typeable
import Data.List (sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Bifunctor (bimap)

class HasImageParser image where
  type Image image
  imageParser :: Int -> Int -> Parser (Image image)
  normalize :: Image image -> Image image

data MatrixImage

instance HasImageParser MatrixImage where
  type Image MatrixImage = [[Double]]
  imageParser w h = P.count h (P.count w (fromIntegral <$> P.anyWord8))
  normalize xs = map (map (/ 255.0)) xs

data FlattenImage

instance HasImageParser FlattenImage where
  type Image FlattenImage = [Double]
  imageParser w h = P.count (w * h) (fromIntegral <$> P.anyWord8)
  normalize xs = map (/ 255.0) xs

class (Binary (Label label), Typeable label) => HasLabelParser label where
  type Label label
  labelParser :: Parser (Label label)

data NumberLabel

instance HasLabelParser NumberLabel where
  type Label NumberLabel = Word8
  labelParser = P.anyWord8

data OneHotLabel

instance HasLabelParser OneHotLabel where
  type Label OneHotLabel = [Word8]
  labelParser = fmap (\w -> map (\v -> if w == v then 1 else 0) [0..9]) P.anyWord8

data Labels label = Labels Int [label] deriving Show
data Images image = Images Int Int Int [image] deriving Show

words2Int :: [Word8] -> Int
words2Int xs = sum $ zipWith (*) (reverse xs') (map (256^) [0..])
  where xs' = map fromIntegral xs

labelsParser :: forall label. HasLabelParser label => Parser (Labels (Label label))
labelsParser = do
  P.string $ BS.pack [0,0,8,1]
  len <- words2Int <$> P.count 4 P.anyWord8
  labels <- P.count len (labelParser @label)
  pure (Labels len labels)

imagesParser :: forall image. HasImageParser image => Parser (Images (Image image))
imagesParser = do
  P.string $ BS.pack [0,0,8,3]
  len    <- words2Int <$> P.count 4 P.anyWord8
  width  <- words2Int <$> P.count 4 P.anyWord8
  height <- words2Int <$> P.count 4 P.anyWord8
  images <- P.count len (imageParser @image width height)
  pure (Images len width height images)

data Training
data Test

class HasLabelFileLocate a where
  labelFilePath :: FilePath
  labelFileRequest :: Request

instance HasLabelFileLocate Test where
  labelFilePath = "tmp/mnist/t10k-labels-idx1-ubyte.gz"
  labelFileRequest = "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"

instance HasLabelFileLocate Training where
  labelFilePath = "tmp/mnist/train-labels-idx1-ubyte.gz"
  labelFileRequest = "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"

class HasImageFileLocate a where
  imageFilePath :: FilePath
  imageFileRequest :: Request

instance HasImageFileLocate Test where
  imageFilePath = "tmp/mnist/t10k-images-idx3-ubyte.gz"
  imageFileRequest = "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"

instance HasImageFileLocate Training where
  imageFilePath = "tmp/mnist/train-images-idx3-ubyte.gz"
  imageFileRequest = "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"

readOrDownloadFile :: FilePath -> Request -> IO BS.ByteString
readOrDownloadFile path req = do
  doesExist <- doesFileExist path
  when (not doesExist) downloadFile
  BL.toStrict . decompress <$> BL.readFile path
   where
    downloadFile :: IO ()
    downloadFile = do
      BS.writeFile path =<< (BL.toStrict . getResponseBody <$> httpLBS req)

getLabels :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label) => IO (Labels (Label label))
getLabels  = do
  content <- readOrDownloadFile (labelFilePath @phase) (labelFileRequest @phase)
  case P.parseOnly (labelsParser @label) content of
    Left msg -> error msg
    Right labels -> pure labels

chunkedIndices :: Int -> [Int] -> IntMap [Int]
chunkedIndices size indices = IntMap.fromListWith (++) $ fmap roundedAndListed $ indices
  where roundedAndListed i = (offset i, [i - offset i])
        offset i = i `div` size * size

labelChunkSize :: Int
labelChunkSize = 100

labelChunksDir :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label) => FilePath
labelChunksDir = dropExtension (labelFilePath @phase) </> labelTypeName
  where labelTypeName = show $ typeRep (Proxy @label)

labelChunkFile :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label) => Int -> FilePath
labelChunkFile index = (labelChunksDir @phase @label) </> show index

chunkifyLabels :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label) => IO [FilePath]
chunkifyLabels = do
  Labels _ labels <- getLabels @phase @label
  let chunks = List.chunksOf labelChunkSize labels
      files = fmap (labelChunkFile @phase @label) $ take (length chunks) [0, labelChunkSize..]
      dir = labelChunksDir @phase @label
  createDirectoryIfMissing True dir
  mapM_ (uncurry Binary.encodeFile) $ zip files chunks
  return files

getLabelsAt :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label)
            => [Int]
            -> IO [Label label]
getLabelsAt indices = concat <$> mapM readContents fileAndIndices
  where
     fileAndIndices = fmap fn $ IntMap.toList $ chunkedIndices labelChunkSize indices
     fn = bimap (labelChunkFile @phase @label) id
     readContents (file, indices) = do
       exists <- doesFileExist file
       when (not exists) $ (chunkifyLabels @phase @label) >> return ()
       labels <- Binary.decodeFile file
       return $ fmap (labels !!) $ sort indices

getImages :: forall phase image. (HasImageFileLocate phase, HasImageParser image) => Bool -> IO (Images (Image image))
getImages doNormalize = do
  content <- readOrDownloadFile (imageFilePath @phase) (imageFileRequest @phase)
  case P.parseOnly (imagesParser @image) content of
    Left msg -> error msg
    Right (Images len width height images) -> pure (Images len width height (if doNormalize then map (normalize @image) images else images))

displayImage :: [[Double]] -> IO ()
displayImage image = mapM_ putStrLn (map (map toGrayscale) image)
  where
    grayscale = " .:-=+*#%@"
    index x = (10 * ceiling x) `quot` 256
    toGrayscale x = grayscale !! (index x)

main :: IO ()
main = do
  (Images _ _ _ images) <- getImages @Test @MatrixImage False
  displayImage (images !! 1)
