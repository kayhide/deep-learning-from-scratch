{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MNIST (
  Images(..),
  MatrixImage,
  Labels(..),
  NumberLabel,
  OneHotLabel,
  Training,
  Test,
  getLabels,
  getLabelsAt,
  getImages,
  getImagesAt,
  displayImage
  ) where

import Codec.Compression.GZip (decompress)
import Control.Monad (when)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Network.HTTP.Simple (Request, httpLBS, getResponseBody)
import qualified Data.List.Extra as List
import Data.List (sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import System.FilePath
import System.Directory (doesFileExist, createDirectoryIfMissing)

class (Binary (Image image)) => HasImageParser image where
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

class (Binary (Label label)) => HasLabelParser label where
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

class HasChunks a b where
  chunksDir :: FilePath

  chunkFile :: Int -> FilePath
  chunkFile offset = (chunksDir @a @b) </> show offset

  chunkSize :: Int
  chunkSize = 100

instance HasChunks Test NumberLabel where
  chunksDir = dropExtension (labelFilePath @Test) </> "NumberLabel"

instance HasChunks Test OneHotLabel where
  chunksDir = dropExtension (labelFilePath @Test) </> "OneHotLabel"

instance HasChunks Training NumberLabel where
  chunksDir = dropExtension (labelFilePath @Training) </> "NumberLabel"

instance HasChunks Training OneHotLabel where
  chunksDir = dropExtension (labelFilePath @Training) </> "OneHotLabel"

instance HasChunks Test MatrixImage where
  chunksDir = dropExtension (imageFilePath @Test) </> "MatrixImage"

instance HasChunks Test FlattenImage where
  chunksDir = dropExtension (imageFilePath @Test) </> "FlattenImage"

instance HasChunks Training MatrixImage where
  chunksDir = dropExtension (imageFilePath @Training) </> "MatrixImage"

instance HasChunks Training FlattenImage where
  chunksDir = dropExtension (imageFilePath @Training) </> "FlattenImage"

readOrDownloadFile :: FilePath -> Request -> IO BS.ByteString
readOrDownloadFile path req = do
  doesExist <- doesFileExist path
  when (not doesExist) downloadFile
  BL.toStrict . decompress <$> BL.readFile path
   where
    downloadFile :: IO ()
    downloadFile = do
      BS.writeFile path =<< (BL.toStrict . getResponseBody <$> httpLBS req)

chunkifyItems :: forall phase item a. (HasChunks phase item, Binary a) => [a] -> IO [FilePath]
chunkifyItems items = do
  let size = (chunkSize @phase @item)
      chunks = List.chunksOf size items
      dir = (chunksDir @phase @item)
      files = fmap ((dir </>) . show) $ take (length chunks) [0, size..]
  createDirectoryIfMissing True dir
  mapM_ (uncurry Binary.encodeFile) $ zip files chunks
  return files

chunkedIndices :: Int -> [Int] -> IntMap [Int]
chunkedIndices size indices = IntMap.fromListWith (++) $ fmap roundedAndListed $ indices
  where roundedAndListed i = (offset i, [i - offset i])
        offset i = i `div` size * size

getLabels :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label) => IO (Labels (Label label))
getLabels  = do
  content <- readOrDownloadFile (labelFilePath @phase) (labelFileRequest @phase)
  case P.parseOnly (labelsParser @label) content of
    Left msg -> error msg
    Right labels -> pure labels

chunkifyLabels :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label, HasChunks phase label)
               => IO [FilePath]
chunkifyLabels = do
  Labels _ labels <- getLabels @phase @label
  chunkifyItems @phase @label labels

getLabelsAt :: forall phase label. (HasLabelFileLocate phase, HasLabelParser label, HasChunks phase label)
            => [Int]
            -> IO [Label label]
getLabelsAt indices = concat <$> mapM readContents offsetAndIndices
  where
     offsetAndIndices = IntMap.toList $ chunkedIndices (chunkSize @phase @label) indices
     readContents (offset, indices) = do
       let file = chunkFile @phase @label offset
       exists <- doesFileExist file
       when (not exists) $ (chunkifyLabels @phase @label) >> return ()
       labels <- Binary.decodeFile file
       return $ fmap (labels !!) $ sort indices


chunkifyImages :: forall phase image. (HasImageFileLocate phase, HasImageParser image, HasChunks phase image)
               => IO [FilePath]
chunkifyImages = do
  Images _ _ _ images <- getImages @phase @image False
  chunkifyItems @phase @image images

getImages :: forall phase image. (HasImageFileLocate phase, HasImageParser image) => Bool -> IO (Images (Image image))
getImages doNormalize = do
  content <- readOrDownloadFile (imageFilePath @phase) (imageFileRequest @phase)
  case P.parseOnly (imagesParser @image) content of
    Left msg -> error msg
    Right (Images len width height images) -> pure (Images len width height (if doNormalize then map (normalize @image) images else images))

getImagesAt :: forall phase image. (HasImageFileLocate phase, HasImageParser image, HasChunks phase image)
            => [Int]
            -> IO [Image image]
getImagesAt indices = concat <$> mapM readContents offsetAndIndices
  where
     offsetAndIndices = IntMap.toList $ chunkedIndices (chunkSize @phase @image) indices
     readContents (offset, indices) = do
       let file = chunkFile @phase @image offset
       exists <- doesFileExist file
       when (not exists) $ (chunkifyImages @phase @image) >> return ()
       images <- Binary.decodeFile file
       return $ fmap (images !!) $ sort indices

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
