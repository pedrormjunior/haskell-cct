
module MatasParallel
       (
         -- * Types
         Point,
         Word8,
         -- * Functions
         matas2D,
         printPTree,
       )
       where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)
import Stack
import Control.Monad (zipWithM_)

import Control.Parallel
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

type Point = (Int, Int)

type Root = Point

und = (-1, -1)

-- mapM_P :: Monad m => (a -> m b) -> [a] -> m ()
mapM_P f xs = do
  mvars <- myReplicate (length xs) newEmptyMVar
  mapM_ (forkIO . fThread) $ zip mvars xs
  mapM_ takeMVar mvars
  where fThread (mvar, l) = f l >> putMVar mvar ()
        myReplicate :: Int -> IO a -> IO [a]
        myReplicate n x = sequence $ replicate n x

matas2D :: V.Vector (V.Vector Word8) -> IO (MV.IOVector (MV.IOVector Point))
matas2D vvec = do
  let (height, width) = size vvec
  nvvec <- MV.new height
  let initialize :: Int -> IO ()
      initialize n | n == height = return ()
                   | otherwise = do
        MV.replicate width und >>= MV.write nvvec n
        initialize $ succ n
    in initialize 0
  let f :: Point -> Word8
      f (i, j) =
        (vvec V.! i) V.! j
      par :: Point -> Point -> IO ()
      par (i, j) p2 = MV.read nvvec i >>= \nvec -> MV.write nvec j p2
      parRead :: Point -> IO Point
      parRead (i, j) = MV.read nvvec i >>= \nvec -> MV.read nvec j
  let matas1D :: Int -> IO ()
      matas1D line =
        let build1D :: Stack Point -> Root -> Point -> IO ()
            build1D s r p
              | snd p == width = finish s r
              | otherwise =
                if f r < f p then build1D (stackPush s r) p next_p
                else if f r == f p then do par p r
                                           build1D s r next_p
                     else if stackEmpty s then do par r p
                                                  build1D s p next_p
                          else if f q < f p then do par r p
                                                    build1D s p next_p
                               else if f q == f p 
                                    then do par r q; par p q
                                            build1D (stackPop s) q next_p
                                    else do par r q
                                            build1D (stackPop s) q p
              where next_p = (fst p, snd p + 1)
                    q = stackLast s
            finish :: Stack Point -> Root -> IO ()
            finish s r | stackEmpty s = par r und
                       | otherwise = do let sl = stackLast s
                                        par r sl
                                        finish (stackPop s) sl
        in build1D emptyStack (line, 0) (line, 1)
    in mapM_P matas1D [0 .. height - 1]
  let merge :: Int -> IO ()
      merge line = zipWithM_ connect
                   [(line,     x) | x <- [0 .. width - 1]]
                   [(line + 1, x) | x <- [0 .. width - 1]]
      connect :: Point -> Point -> IO ()
      connect x y = do
        levroot_x <- levroot x
        levroot_y <- levroot y
        if f levroot_y > f levroot_x
          then connect' levroot_y levroot_x
          else connect' levroot_x levroot_y
        where connect' :: Point -> Point -> IO ()
              connect' x y | x == y = return ()
                           | otherwise = do
                par_x <- parRead x
                if par_x == und then par x y
                  else do
                  z <- levroot par_x
                  if f z > f y then connect' z y
                    else do par x y
                            connect' y z
      levroot :: Point -> IO Point
      levroot x = do
        par_x <- parRead x
        if par_x == und then return x else
          if f x == f par_x then do levroot_par_x <- levroot par_x
                                    par x levroot_par_x
                                    return levroot_par_x
          else return x
      mergeAll :: [[Int]] -> IO ()
      mergeAll = mapM_ $ mapM_P merge
    in mergeAll $ linesToMerge $ height - 1
  return nvvec

linesToMerge :: Int -> [[Int]]
linesToMerge n = takeWhile (not . null) $ map f [1 ..]
  where f :: Int -> [Int]
        f m = takeWhile (< n) $ iterate (+2^m) (initial m)
        initial :: Int -> Int
        initial n = 2^(n-1) - 1

size :: V.Vector (V.Vector Word8) -> (Int, Int)
size vvec = (V.length vvec, V.length $ vvec V.! 0)

printPTree :: MV.IOVector (MV.IOVector Point) -> IO ()
printPTree pt = new_pt >>=
                V.unsafeFreeze >>=
                V.mapM_ (\mv -> MV.clone mv >>= V.unsafeFreeze >>= print)
  where new_pt = MV.clone pt

testfunc = do
  let v1 = V.fromList [7, 9, 1, 2, 4, 3, 9, 2, 5, 6]
      v2 = V.fromList [5, 1, 7, 4, 3, 3, 9, 7, 2, 6]
      v3 = V.fromList [6, 3, 8, 1, 5, 9, 4, 4, 1, 8]
      vv = V.fromList [v1, v2, v3]
  nvvec <- matas2D vv
  printPTree nvvec
  return ()

auxfunc :: IO ()
auxfunc = do
  aux <- MV.new 10
  MV.write aux 0 10
  MV.write aux 2 11
  MV.read aux 0 >>= print
  MV.read aux 2 >>= print
  MV.read aux 3 >>= print       -- Causa erro
  return ()
