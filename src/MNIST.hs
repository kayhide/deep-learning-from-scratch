{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MNIST (
  Image,
  FlattenImage,
  Images(..),
  Label,
  OneHotLabel,
  Labels(..),
  Phase(..),
  Sample,
  getLabels,
  getLabelsAt,
  getImages,
  getImagesAt,
  displayImage,
  getSamples,
  displaySample
  ) where

import Codec.Compression.GZip (decompress)
import Control.Monad (when, guard, void)
import Data.Bifunctor
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Network.HTTP.Simple (Request, httpLBS, getResponseBody)
import Network.HTTP.Conduit (path)
import qualified Data.List.Extra as List
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.Yaml as Yaml
import System.FilePath
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Random

type Image = [[Double]]
type FlattenImage = [Double]

type Label = Word8
type OneHotLabel = [Word8]

toOneHotLabel :: Label -> OneHotLabel
toOneHotLabel label = replicate n 0 ++ 1 : replicate (9 - n) 0
  where n = fromIntegral label

data Labels = Labels Int [Label] deriving Show
data Images = Images Int Int Int [Image] deriving Show

words2Int :: [Word8] -> Int
words2Int xs = sum $ zipWith (*) (reverse xs') (map (256^) [0..])
  where xs' = map fromIntegral xs

labelsParser :: Parser Labels
labelsParser = do
  P.string $ BS.pack [0,0,8,1]
  len <- words2Int <$> P.count 4 P.anyWord8
  labels <- P.count len labelParser
  pure (Labels len labels)
  where
    labelParser = P.anyWord8

imagesParser :: Parser Images
imagesParser = do
  P.string $ BS.pack [0,0,8,3]
  len    <- words2Int <$> P.count 4 P.anyWord8
  width  <- words2Int <$> P.count 4 P.anyWord8
  height <- words2Int <$> P.count 4 P.anyWord8
  images <- P.count len (imageParser width height)
  pure (Images len width height images)
  where
    imageParser w h = P.count h (P.count w (fromIntegral <$> P.anyWord8))

data Phase = Test | Training deriving (Show, Eq, Ord)

labelsUrl :: Phase -> Request
labelsUrl Test = "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"
labelsUrl Training = "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"

imagesUrl :: Phase -> Request
imagesUrl Test = "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"
imagesUrl Training = "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"

type LabelsMeta = Map String Int
type ImagesMeta = Map String Int

chunkSize :: Int
chunkSize = 100

metaFile :: String
metaFile = "meta.yaml"

readOrDownloadFile :: Request -> IO ByteString
readOrDownloadFile req = do
  doesExist <- doesFileExist file
  when (not doesExist) downloadFile
  BL.toStrict . decompress <$> BL.readFile file
   where
     file = downloadedFile req
     downloadFile :: IO ()
     downloadFile = do
       createDirectoryIfMissing True (takeDirectory file)
       BS.writeFile file =<< (BL.toStrict . getResponseBody <$> httpLBS req)

downloadedFile :: Request -> FilePath
downloadedFile req = "tmp" </> "mnist" </> (takeFileName . unpack . path) req

writeMeta :: (Yaml.ToJSON a) => FilePath -> a -> IO FilePath
writeMeta dir x = do
  let file = dir </> metaFile
  createDirectoryIfMissing True dir
  Yaml.encodeFile file x
  return file

readMeta :: (Yaml.FromJSON a) => FilePath -> IO (Maybe a)
readMeta dir = do
  let file = dir </> metaFile
  guard =<< doesFileExist file
  Yaml.decodeFile file

chunkifyItems :: (Binary a) => FilePath -> [a] -> IO [FilePath]
chunkifyItems dir items = do
  let size = chunkSize
      chunks = List.chunksOf size items
      files = fmap ((dir </>) . show) $ take (length chunks) [0, size..]
  createDirectoryIfMissing True dir
  mapM_ (uncurry Binary.encodeFile) $ zip files chunks
  return files

chunkedIndices :: Int -> [Int] -> IntMap [Int]
chunkedIndices size indices = IntMap.fromListWith (++) $ fmap prepare indices
  where
    prepare i = (bimap (*size) (:[])) $ i `divMod` size

getLabels :: Phase -> IO Labels
getLabels phase = do
  content <- readOrDownloadFile req
  case P.parseOnly labelsParser content of
    Left msg -> error msg
    Right labels -> pure labels
  where
    req = labelsUrl phase

chunkedLabelsDir :: Phase -> FilePath
chunkedLabelsDir = dropExtension . downloadedFile . labelsUrl

chunkifyLabels :: Phase -> IO [FilePath]
chunkifyLabels phase = do
  Labels len labels <- getLabels phase
  writeMeta dir (Map.fromList [("count", len)] :: (Map String Int))
  chunkifyItems dir labels
  where
    dir = chunkedLabelsDir phase

getLabelsAt :: Phase -> [Int] -> IO [Label]
getLabelsAt phase indices = concat <$> mapM readContents offsetAndIndices
  where
    dir = chunkedLabelsDir phase
    offsetAndIndices = IntMap.toList $ chunkedIndices chunkSize indices
    readContents (offset, indices) = do
      let file = dir </> show offset
      exists <- doesFileExist file
      when (not exists) $ void $ chunkifyLabels phase
      labels <- Binary.decodeFile file
      return $ fmap (labels !!) $ sort indices

getLabelsMeta :: Phase -> IO (Maybe LabelsMeta)
getLabelsMeta = readMeta . chunkedLabelsDir

getImages :: Phase -> IO Images
getImages phase = do
  content <- readOrDownloadFile $ imagesUrl phase
  case P.parseOnly imagesParser content of
    Left msg -> error msg
    Right images -> return images

chunkedImagesDir :: Phase -> FilePath
chunkedImagesDir = dropExtension . downloadedFile . imagesUrl

chunkifyImages :: Phase -> IO [FilePath]
chunkifyImages phase = do
  Images len width height images <- getImages phase
  writeMeta dir (Map.fromList [("count", len), ("width", width), ("height", height)] :: (Map String Int))
  chunkifyItems dir images
  where
    dir = chunkedImagesDir phase

getImagesAt :: Phase -> [Int] -> IO [Image]
getImagesAt phase indices = concat <$> mapM readContents offsetAndIndices
  where
    dir = chunkedImagesDir phase
    offsetAndIndices = IntMap.toList $ chunkedIndices chunkSize indices
    readContents (offset, indices) = do
      let file = dir </> show offset
      exists <- doesFileExist file
      when (not exists) $ void $ chunkifyImages phase
      images <- Binary.decodeFile file
      return $ fmap (images !!) $ sort indices

getImagesMeta :: Phase -> IO (Maybe ImagesMeta)
getImagesMeta = readMeta . chunkedImagesDir

displayImage :: Image -> IO ()
displayImage image = mapM_ putStrLn (map (map toGrayscale) image)
  where
    grayscale = " .:-=+*#%@"
    index x = (10 * ceiling x) `quot` 256
    toGrayscale x = grayscale !! (index x)

type Sample = (Label, Image)

getSamples :: Phase -> Int -> IO [Sample]
getSamples phase n = do
  Just meta <- getLabelsMeta phase
  let Just count = Map.lookup "count" meta
  gen <- newStdGen
  let indices = List.sort $ take n $ List.nub $ randomRs (0, count - 1) gen
  zip <$> (getLabelsAt phase indices) <*> (getImagesAt phase indices)

displaySample :: Sample -> IO ()
displaySample (num, image) = do
  displayImage image
  print num


main :: IO ()
main = do
  samples <- getSamples Test 5
  mapM_ displaySample samples
