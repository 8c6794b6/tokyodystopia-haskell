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
import Data.Int (Int64)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), hasExtension)
import Test.QuickCheck
import qualified Data.ByteString.Char8 as C8
import qualified Test.QuickCheck.Monadic as QM

import Database.TokyoDystopia
import qualified Database.TokyoCabinet.List as TCL
import qualified Database.TokyoDystopia as TD
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB
import qualified Database.TokyoDystopia.WDB as WDB
import qualified Database.TokyoDystopia.Utils as U

main :: IO ()
main = do
  results <- mapM quickCheckResult
    [label "idb" prop_idb
    ,label "qdb" prop_qdb
    ,label "jdb" prop_jdb
    ,label "wdb" prop_wdb
    ,label "class_idb" prop_class_idb
    ,label "class_qdb" prop_class_qdb
    ,label "class_jdb" prop_class_jdb
    ,label "class_wdb" prop_class_wdb]
  if any (not . isSuccess) results then exitFailure else exitSuccess

prop_idb :: Property
prop_idb =
  forAll (arbitrary `suchThat` uncurry validKS) $ \(key, val) ->
  QM.monadicIO $ do
    dbPath <- QM.run $ getTDDir "casket"
    (val',ids,ec,rnum,fs,opt) <- QM.run $
      U.withIDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
        IDB.put db (key::Int) (C8.pack val)
        val' <- IDB.get db key
        ids <- IDB.search db val [GMSUBSTR]
        ec <- IDB.ecode db -- Other code than ESUCCESS?
        rnum <- IDB.rnum db
        fs <- IDB.fsiz db
        opt <- IDB.optimize db
        return (val',ids,ec,rnum,fs,opt)
    QM.assert $
      maybe "" C8.unpack val' == val && not (null ids) && opt

prop_qdb :: Property
prop_qdb = forAll (arbitrary `suchThat` (> 0)) $ \key ->
  QM.monadicIO $ do
    val <- QM.pick asciis
    QM.pre $ validKS (key::Int) val
    dbPath <- QM.run $ getTDDir "casket.tcq"
    (res,ids,ec,tn,fs,opt) <- QM.run $
      U.withQDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
        res <- QDB.put db key (C8.pack val)
        ids <- QDB.search db val [GMSUBSTR]
        ec <- QDB.ecode db
        tn <- QDB.tnum db
        fs <- QDB.fsiz db
        opt <- QDB.optimize db
        return (res,ids,ec,tn,fs,opt)
    QM.assert $
      res && (not $ null ids) && opt
      --  && tn > 1 &&  opt && ec == ESUCCESS && fs > 1

prop_jdb :: Property
prop_jdb = forAll ((arbitrary :: Gen Int) `suchThat` (> 0)) $ \key ->
  QM.monadicIO $ do
    vals <- QM.pick (listOf1 asciis)
    dbPath <- QM.run $ getTDDir "laputa"
    l <- QM.run $ TCL.new >>= \l -> mapM_ (TCL.push l) vals >> return l
    (res,ids,ec,tn,fs,opt) <- QM.run $
      U.withJDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
        JDB.put db key l
        res <- TCL.dump =<< JDB.get db key
        ids <- JDB.search2 db (head vals)
        ec <- JDB.ecode db
        tn <- JDB.rnum db
        fs <- JDB.fsiz db
        opt <- JDB.optimize db
        return (res,ids,ec,tn,fs,opt)
    QM.assert $
      C8.length res > 0 && length ids > 0 && opt
      -- && ec == ESUCCESS && tn > 1 && fs > 1

prop_wdb :: Property
prop_wdb =
  forAll ((arbitrary::Gen Int) `suchThat` (> 0)) $ \key ->
  QM.monadicIO $ do
    vals <- QM.pick (listOf1 asciis)
    QM.pre $ key > 0
    dbPath <- QM.run $ getTDDir "casket.tcw"
    l <- QM.run $ TCL.new >>= \l -> mapM_ (TCL.push l . C8.pack) vals >> return l
    (res,ids,ec,tn,fs,opt) <- QM.run $
      U.withWDB dbPath [OCREAT,OWRITER,OREADER] $ \db -> do
        res <- WDB.put db key l
        ids <- WDB.search db (head vals)
        ec <- WDB.ecode db
        tn <- WDB.tnum db
        fs <- WDB.fsiz db
        opt <- WDB.optimize db
        return (res,ids,ec,tn,fs,opt)
    QM.assert $ res && not (null ids) && opt

prop_class_idb :: Property
prop_class_idb =
  forAll (arbitrary `suchThat` uncurry validKS) $ \(key,val) ->
  QM.monadicIO $ do
    dbPath <- QM.run $ getTDDir "casket2"
    (val',dbPath') <- QM.run $ TD.runTDM $ do
      db <- TD.new :: TDM IDB
      TD.tune db 1000000 1000000 53687 [TLARGE,TTCBS]
      TD.open db dbPath [OWRITER,OCREAT,OWRITER]
      TD.put db key (C8.pack val)
      val' <- TD.get db key
      dbPath' <- TD.path db
      TD.close db >> TD.del db
      return (val',dbPath')
    QM.assert $ (maybe "" C8.unpack val') == val && dbPath == dbPath'

prop_class_qdb :: Property
prop_class_qdb =
  forAll (arbitrary `suchThat` (> 0)) $ \key ->
  QM.monadicIO $ do
    val <- QM.pick asciis
    dbPath <- QM.run $ getTDDir "casket2.tcq"
    (res,ids,dbPath') <- QM.run $ TD.runTDM $ do
      db <- TD.new :: TDM QDB
      TD.tune db 1000000 1000000 5368 [TLARGE,TBZIP]
      TD.open db dbPath [OWRITER,OCREAT,OWRITER,ONOLCK]
      res <- TD.put db key (C8.pack val)
      ids <- TD.search db val [GMSUBSTR]
      dbPath' <- TD.path db
      TD.close db >> TD.del db
      return (res,ids,dbPath')
    QM.assert $ res && not (null ids) && dbPath == dbPath'

-- prop_class_jdb :: Int64 -> Property
-- prop_class_jdb key = QM.monadicIO $ do
prop_class_jdb :: Property
prop_class_jdb =
  forAll (arbitrary `suchThat` uncurry validKSs) $ \(key, vals) ->
  QM.monadicIO $ do
    dbPath <- QM.run $ getTDDir "laputa2"
    l <- QM.run $ TCL.new >>= \l -> mapM_ (TCL.push l . C8.pack) vals >> return l
    (res,ids,dbPath') <- QM.run $ TD.runTDM $ do
      db <- TD.new :: TDM JDB
      TD.tune db 1000000 1000000 536870912 [TDEFLATE]
      TD.open db dbPath [OWRITER,OCREAT,OREADER,OLCKNB]
      res <- TD.put db key l
      ids <- TD.search db (head vals) [GMSUBSTR]
      dbPath' <- TD.path db
      TD.close db >> TD.del db
      return (res,ids,dbPath')
    QM.assert $ res && {- not (null ids) && -} dbPath == dbPath'

prop_class_wdb :: Int64 -> Property
prop_class_wdb key = QM.monadicIO $ do
-- prop_class_wdb :: Property
-- prop_class_wdb =
  vals <- QM.pick (listOf1 asciis)
  QM.pre $ validKSs key vals
  dbPath <- QM.run $ getTDDir "casket2.tcw"
  l <- QM.run $ TCL.new >>= \l -> mapM_ (TCL.push l . C8.pack) vals >> return l
  (res,ids,dbPath') <- QM.run $ TD.runTDM $ do
    db <- TD.new :: TDM WDB
    TD.tune db 100000 100000 53687091 [TEXCODEC]
    TD.open db dbPath [OWRITER,OCREAT,OREADER]
    res <- TD.put db key l
    ids <- TD.search db (head vals) [GMSUBSTR]
    dbPath' <- TD.path db
    TD.close db >> TD.del db
    return (res,ids,dbPath')
  QM.assert $ res && not (null ids) && dbPath == dbPath'

--
-- Utils
--

isSuccess :: Result -> Bool
isSuccess r = case r of Success _ _ _ -> True; _ -> False

validKS :: (Ord a, Num a) => a -> String -> Bool
validKS k v =
  not (null $ filter (\x -> x /= ' ' && x /= '\n') v) &&
  (not $ ('\\' `elem` v || '\NUL' `elem` v)) &&
  k > 1

validKSs :: (Ord a, Num a) => a -> [[b]] -> Bool
validKSs k vs = not (null vs) && all (not . null) vs && k > 1

asciis :: Gen String
asciis = listOf1 (elements $ ['A'..'Z'] ++ ['a'..'z'])

getTDDir :: FilePath -> IO FilePath
getTDDir baseName = do
  filePath <- (</> "td" </> baseName) `fmap` getTemporaryDirectory
  unless (hasExtension baseName) (createDirectoryIfMissing True filePath)
  return filePath
