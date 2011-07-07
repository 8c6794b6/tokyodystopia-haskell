{-# LANGUAGE PackageImports, OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Main test
--
module Main where

import Control.Monad
import Control.Monad.Trans ( liftIO )
import Data.Int (Int64)
import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import Test.QuickCheck (Property, (==>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as QM

import Database.TokyoDystopia
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoCabinet.List as TCL
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB
import qualified Database.TokyoDystopia.WDB as WDB
import qualified Database.TokyoDystopia.Utils as U

main :: IO ()
main = do
  -- results <- mapM (Q.quickCheckWithResult (Q.Args Nothing 1000 10000 100 True))
  results <- mapM Q.quickCheckResult
    [Q.label "idb" prop_idb
    ,Q.label "qdb" prop_qdb
    ,Q.label "jdb" prop_jdb
    ,Q.label "wdb" prop_wdb]
  if any (not . isSuccess) results then exitFailure else exitSuccess

prop_idb :: Int64 -> String -> Property
prop_idb key val = QM.monadicIO $ do
  QM.pre $ validKS key val
  dbPath <- QM.run $ do
    path <- (</> "casket") `fmap` getTemporaryDirectory
    createDirectoryIfMissing True path
    return path
  (val',ids) <- QM.run $ U.withIDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
    IDB.put db key (C8.pack val)
    val' <- IDB.get db key
    ids <- IDB.search db val [GMSUBSTR]
    return (val',ids)
  QM.assert (maybe "" C8.unpack val' == val && not (null ids))

prop_qdb :: Int64 -> Property
prop_qdb key = QM.monadicIO $ do
  val <- QM.pick asciis
  QM.pre $ validKS key val
  dbPath <- QM.run $ (</> "casket.tcq") `fmap` getTemporaryDirectory
  (res,ids) <- QM.run $ U.withQDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
    res <- QDB.put db key (C8.pack val)
    ids <- QDB.search db val [GMSUBSTR]
    return (res,ids)
  QM.assert $ res && (not $ null ids)

prop_jdb :: Int -> Property
prop_jdb key = QM.monadicIO $ do
  vals <- QM.pick (Q.listOf1 asciis)
  QM.pre $ key > 0 && not (null vals) && all (not . null) vals
  dbPath <- QM.run $ do
    path <- (</> "laputa") `fmap` getTemporaryDirectory
    createDirectoryIfMissing True path
    return path
  l <- QM.run $ TCL.new >>= \l ->
    mapM_ (TCL.push l) vals >> return l
  (res,ids) <- QM.run $ U.withJDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
    JDB.put db key l
    res <- TCL.dump =<< JDB.get db key
    ids <- JDB.search2 db (head vals)
    return (res,ids)
  QM.assert $ C8.length res > 0 && length ids > 0

prop_wdb :: Int64 -> Property
prop_wdb key = QM.monadicIO $ do
  vals <- QM.pick (Q.listOf1 asciis)
  QM.pre $ key > 0
  dbPath <- QM.run $ (</> "casket.tcw") `fmap` getTemporaryDirectory
  l <- QM.run $ TCL.new >>= \l ->
    mapM_ (TCL.push l . C8.pack) vals >> return l
  (res,ids) <- QM.run $ U.withWDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
    res <- WDB.put db key l
    ids <- WDB.search db (head vals)
    return (res,ids)
  QM.assert $ res && length ids > 0

-- putThenGet :: TD.TDDB db val
--            => db -> FilePath -> Int64 -> val -> TDM (Maybe val)
-- putThenGet db dbPath key val = do
--   TD.open db dbPath [OCREAT,OWRITER,OREADER]
--   TD.put db key val
--   val' <- TD.get db key
--   TD.close db >> TD.del db
--   return val'

isSuccess :: Q.Result -> Bool
isSuccess r = case r of Q.Success _ _ _ -> True; _ -> False

validKS :: (Ord a, Num a) => a -> String -> Bool
validKS k v =
  not (null $ filter (\x -> x /= ' ' && x /= '\n') v) &&
  (not $ ('\\' `elem` v || '\NUL' `elem` v)) &&
  k > 1

asciis :: Q.Gen String
asciis = Q.listOf1 (Q.elements $ ['A'..'Z'] ++ ['a'..'z'])


------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- From here starts old codes. Not using quickcheck.
--

-- main :: IO ()
-- main = do
--   IDB tests
--   idb <- TD.runTDM $ do
--            a <- test_read_idb
--            b <- test_write_idb
--            c <- test_read_idb_2 1
--            d <- test_search_idb "united"
--            e <- test_mtime_idb
--            return (a,b,c,d,e)
--   putStrLn "idb tests:" >> print idb

--   QDB tests
--   qdb <- TD.runTDM $ do
--            a <- test_write_qdb
--            b <- test_read_qdb
--            c <- test_search_qdb "a"
--            return (a, b, c)
--   putStrLn "qdb tests:" >> print qdb

--   JDB tests
--   jdb <- TD.runTDM $ do
--            a <- test_write_jdb
--            b <- test_read_jdb
--            c <- test_search_qdb "a"
--            return (a, b, c)
--   putStrLn "jdb tests:" >> print jdb

--   WDB tests
--   wdb <- TD.runTDM $ do
--           a <- test_write_wdb
--           b <- test_read_wdb
--           c <- test_search_wdb "foo"
--           return (a, b, c)
--   putStrLn "wdb tests:" >> print wdb

------------------------------------------------------------------------------
--
-- IDB
--
------------------------------------------------------------------------------

idbPath :: FilePath
idbPath = "test/idb/casket"

test_read_idb :: TDM (Maybe ByteString)
test_read_idb = do
  db <- TD.new :: TDM IDB
  TD.open db idbPath [OWRITER]
  val <- TD.get db 1
  TD.close db >> TD.del db
  return val

test_write_idb :: TDM Bool
test_write_idb = do
  db <- TD.new :: TDM IDB
  TD.open db idbPath [OCREAT, OWRITER]
  res <- mapM (uncurry $ TD.put db)
       [ (1, "foo")
       , (2, "bar")
       , (3, "buzz") ]
  TD.close db >> TD.del db
  return $ and res

test_read_idb_2 :: Int64 -> TDM (Maybe ByteString)
test_read_idb_2 key = do
  db <- TD.new :: TDM IDB
  TD.open db idbPath [OREADER]
  res <- TD.get db key
  TD.close db >> TD.del db
  return res

test_search_idb :: String -> TDM [(Int64, ByteString)]
test_search_idb query = do
  db <- (TD.new :: TDM IDB)
  TD.open db idbPath [OREADER]
  ks <- TD.search db query [GMSUBSTR]
  res <- mapM (\k -> TD.get db k >>= \(Just v) -> return (k,v)) ks
  TD.close db >> TD.del db
  return res

test_mtime_idb :: TDM String
test_mtime_idb = do
  db <- (TD.new :: TDM IDB)
  TD.open db idbPath [OREADER]
  t <- liftIO $ IDB.mtime db
  return $ show t

------------------------------------------------------------------------------
--
-- QDB
--
------------------------------------------------------------------------------

qdbPath :: FilePath
qdbPath = "test/casket.tcq"

test_write_qdb :: TDM Bool
test_write_qdb = do
  db <- TD.new :: TDM QDB
  TD.open db qdbPath [OWRITER, OCREAT]
  res <- mapM (uncurry $ TD.put db)
         [(1, "hello"),
          (2, "haskell"),
          (3, "tokyo"),
          (4, "dystopia"),
          (5, "qdb")]
  TD.close db >> TD.del db
  return $ and res

test_read_qdb :: TDM (Maybe ByteString)
test_read_qdb = do
  db <- TD.new :: TDM QDB
  TD.open db qdbPath [OREADER]
  res <- TD.get db 1
  TD.close db >> TD.del db
  return $ res

test_search_qdb :: String -> TDM [Int64]
test_search_qdb query = do
  db <- TD.new :: TDM QDB
  TD.open db qdbPath [OREADER]
  res <- TD.search db query [GMSUBSTR]
  TD.close db >> TD.del db
  return res

test_idset_qdb :: IO ()
test_idset_qdb = do
  set <- QDB.idsetnew 32
  QDB.idsetmark set 3
  print =<< QDB.idsetcheck set 3
  print =<< QDB.idsetcheck set 4
  QDB.idsetclear set
  print =<< QDB.idsetcheck set 3
  QDB.idsetdel set

test_tn_qdb :: IO ()
test_tn_qdb = do
  undefined

------------------------------------------------------------------------------
--
-- JDB
--
------------------------------------------------------------------------------

jdbPath :: FilePath
jdbPath = "test/laputa"

test_write_jdb :: TDM Bool
test_write_jdb = do

  db <- TD.new :: TDM JDB
  TD.open db jdbPath [OCREAT, OWRITER]

  l1 <- liftIO (TCL.new :: IO (TCL.List ByteString))
  l2 <- liftIO (TCL.new :: IO (TCL.List ByteString))
  l3 <- liftIO (TCL.new :: IO (TCL.List ByteString))

  liftIO $ mapM_ (TCL.push l1 . C8.pack) ["apple", "ant", "antenna"] >>
           mapM_ (TCL.push l2 . C8.pack) ["banana", "bear", "bubble"] >>
           mapM_ (TCL.push l3 . C8.pack) ["cherry", "chair", "chocolate"]

  r1 <- TD.put db 1 l1
  r2 <- TD.put db 2 l2
  r3 <- TD.put db 3 l3

  TD.close db >> TD.del db >> liftIO (mapM_ TCL.delete [l1,l2,l3])

  return $ and [r1, r2, r3]


test_read_jdb :: TDM ByteString
test_read_jdb = do
  db <- TD.new :: TDM JDB
  TD.open db jdbPath [OREADER]
  res <- TD.get db 2
  res' <- liftIO $ maybe (return B.empty) TCL.dump res
  TD.close db >> TD.del db
  return res'


test_search_jdb :: String -> TDM [Int64]
test_search_jdb q = do
  db <- TD.new :: TDM JDB
  TD.open db jdbPath [OREADER]
  res <- TD.search db q [GMSUBSTR]
  TD.close db >> TD.del db
  return res


------------------------------------------------------------------------------
--
-- WDB
--
------------------------------------------------------------------------------

wdbPath :: FilePath
wdbPath = "test/casket.tcw"

test_write_wdb :: TDM Bool
test_write_wdb = do
  db <- TD.new :: TDM WDB
  TD.open db wdbPath [OCREAT, OWRITER]
  l1 <- liftIO $ TCL.new
  l2 <- liftIO $ TCL.new
  l3 <- liftIO $ TCL.new
  liftIO $ do
    mapM_ (TCL.push l1 . C8.pack) ["foo", "bar", "buzz"]
    mapM_ (TCL.push l2 . C8.pack) ["foo", "bar", "apple"]
    mapM_ (TCL.push l3 . C8.pack) ["foo", "apple", "banana"]
  r1 <- TD.put db 1 l1
  r2 <- TD.put db 2 l2
  r3 <- TD.put db 3 l3
  TD.close db
  TD.del db
  return $ and [r1, r2, r3]

-- | TD.get for WDB will always return empty ByteString.
test_read_wdb :: TDM (ByteString)
test_read_wdb = do
  db <- TD.new :: TDM WDB
  TD.open db wdbPath [OREADER]
  res <- TD.get db 1
  res' <- liftIO $ maybe (return B.empty) TCL.dump res
  TD.close db >> TD.del db
  return res'

test_search_wdb :: String -> TDM [Int64]
test_search_wdb q = do
  db <- TD.new :: TDM WDB
  TD.open db wdbPath [OREADER]
  ks <- TD.search db q [GMSUBSTR]
  TD.close db >> TD.del db
  return ks