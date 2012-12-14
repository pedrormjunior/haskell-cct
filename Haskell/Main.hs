
module Main
       (
         -- * Funtions
         toGray,
         toVVector,
         main,
       )
       where

import Data.Array.Repa.IO.BMP
import Data.Array.Repa hiding
  (
    map,
    slice,
  )
import System.Environment (getArgs)
import Data.Word (Word8)

import Data.Vector hiding
  (
    map,
    toList,
    takeWhile,
  )
import qualified Data.Vector as V

import qualified MatasSequential as MS
import qualified MatasParallel as MP

import Data.Time.Clock

toGray :: [(Word8, Word8, Word8)] -> [Word8]
toGray = map rgb2gray
  where rgb2gray (r, g, b) = ceiling $
                             0.21 * (fromIntegral r) +
                             0.71 * (fromIntegral g) +
                             0.08 * (fromIntegral b)

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f c b a = f a b c

toVVector :: Int -> Int -> Vector Word8 -> Vector (Vector Word8)
toVVector h w v = V.map (flip3 slice v w) froms
  where froms = V.fromList $ takeWhile (< h*w) [0, w ..]

runMatas matas2D img = do
  a <- getCurrentTime
  ptree <- matas2D img
  b <- getCurrentTime
  return $ diffUTCTime b a

mainAll file_name = do
  (Right rimg) <- readImageFromBMP file_name
  let (Z :. h :. w) = extent rimg
  let limg = toGray $ toList rimg
  let vimg = V.fromList limg
  let img = toVVector h w vimg
  return img

mainSequential file_name = do
  img <- mainAll file_name
  runMatas MS.matas2D img

mainParallel file_name = do
  img <- mainAll file_name
  runMatas MP.matas2D img

-- |
main = do
  args <- getArgs
  exec_time <- case args !! 0 of
    "par" -> mainParallel $ args !! 1 
    "seq" -> mainSequential $ args !! 1
  print exec_time
