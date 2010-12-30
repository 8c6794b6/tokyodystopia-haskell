------------------------------------------------------------------------------
-- |
-- Module      : Database.TokyoDystopia.Internal
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal helper functions.
--

module Database.TokyoDystopia.Internal
    ( bitOr
    , toTuningOptions
    , powerOfTwos
    , mkOpen
    , mkSearch
    , mkSearch2
    ) where

import Data.Bits (Bits, (.|.))
import Foreign ( Ptr, Storable )
import Foreign.C.Types ( CInt )
import Foreign.C.String ( CString )
import Database.TokyoDystopia.Types ( OpenMode(..), TuningOption(..) )
import qualified Foreign as FG
import qualified Foreign.C.String as CS

-- | Bitwise or for bits.
bitOr :: (Bits a) => [a] -> a
bitOr = foldr (.|.) 0

-- | Helper function to get list of TuningOption.
toTuningOptions :: Int -> [TuningOption]
toTuningOptions o = map f $ powerOfTwos o
  where
    f x = case x of
      1 -> TLARGE
      2 -> TDEFLATE
      4 -> TBZIP
      8 -> TTCBS
      _ -> error $ "Unknown tuning option: " ++ show x

-- | Returns power of 2 s to construct the number.
--
-- > > sum . map (2^) $ powerOfTwos 111
-- > 111
powerOfTwos :: Int -> [Int]
powerOfTwos = foldr f [] . toBits
  where f (a,b) xs = if b then a:xs else xs

-- | Returns a list containig exponent of 2 to construct the number
toBits :: Int -> [(Int, Bool)]
toBits a = toB (mgp a 2 - 1) [] a
  where
    toB :: Int -> [(Int,Bool)] -> Int -> [(Int,Bool)]
    toB n os x
      | n < 0     = os
      | otherwise = if x >= 2 ^ n
            then toB (n-1) ((n,True):os) (x-2^n)
            else toB (n-1) ((n,False):os) x

-- | Minimum exponent exceeding given value
mgp :: Int -- ^ target
    -> Int -- ^ number to compare with increasing the exponent
    -> Int
mgp a b = go 0 a b
  where
    go :: Int -> Int -> Int -> Int
    go n x p
      | p < 0      = 0
      | x >= p ^ n = go (n+1) x p
      | otherwise  = n


-- | Helper function for opening database.
mkOpen :: (Ptr a -> CString -> CInt -> IO Bool)
       -> (b -> Ptr a)
       -> (OpenMode -> CInt)
       -> b
       -> FilePath -> [OpenMode] -> IO Bool
mkOpen dbFunc unDB modeFunc = \db path modes ->
    CS.withCString path $ \path' ->
        dbFunc (unDB db) path' (bitOr $ fmap modeFunc modes)

-- | Helper function for searching database.
mkSearch :: (Integral a, Storable a, Storable a1)
         => (db -> CString -> t3 -> Ptr a -> IO (Ptr a1))
         -> (t -> db)
         -> (t1 -> t3)
         -> t
         -> String
         -> t1
         -> IO [a1]
mkSearch searchFunc unDB modeFunc db query modes =
  FG.with 0 $ \counterP ->
    CS.withCString query $ \query' -> do
      res <- searchFunc (unDB db) query' (modeFunc modes) counterP
      numResult <- fromIntegral `fmap` FG.peek counterP
      res' <- FG.peekArray numResult res
      FG.free res
      return res'

-- | Another helper function for searching.
mkSearch2 :: (Integral a, Storable a, Storable a1)
          => (t1 -> CString -> Ptr a -> IO (Ptr a1))
          -> (t -> t1)
          -> t
          -> String
          -> IO [a1]
mkSearch2 searchFunc unDB = \db query -> do
  FG.with 0 $ \counterP ->
    CS.withCString query $ \query' -> do
      res <- searchFunc (unDB db) query' counterP
      numResult <- fromIntegral `fmap` FG.peek counterP
      res' <- FG.peekArray numResult res
      FG.free res
      return res'
