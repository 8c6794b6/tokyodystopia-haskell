------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia full text search.
-- For more information about tokyo dystopia, visit:
--
-- * <http://fallabs.com/tokyodystopia/>
--
-- /Examples/:
--
-- Doing put and get of ByteString value with IDB:
--
-- > import qualified Data.ByteString.Char8 as C8
-- > import qualified Database.TokyoDystopia as TD
-- >
-- > main :: IO ()
-- > main = do
-- >   foo <- TD.runTDM $ do
-- >     db <- TD.new :: TD.TDM TD.IDB
-- >     TD.open db "casket" [TD.OCREAT, TD.OWRITER]
-- >     TD.put db 1 (C8.pack "foo")
-- >     result <- TD.get db 1
-- >     TD.close db
-- >     return result
-- >   print foo
--
-- Searching IDB database:
--
-- > import Control.Monad (zipWithM_)
-- > import qualified Data.ByteString.Char8 as C8
-- > import qualified Database.TokyoDystopia as TD
-- >
-- > main :: IO ()
-- > main = do
-- >   vals <- C8.lines `fmap` C8.readFile "/etc/group"
-- >   keys <- TD.runTDM $ do
-- >     db <- TD.new :: TD.TDM TD.IDB
-- >     TD.open db "casket" [TD.OCREAT, TD.OWRITER]
-- >     zipWithM_ (TD.put db) [1..] vals
-- >     result <- TD.search db "root" [TD.GMSUBSTR]
-- >     TD.close db
-- >     return result
-- >   print keys
--
module Database.TokyoDystopia
    ( IDB.IDB
    , QDB.QDB
    , JDB.JDB
    , WDB.WDB
    , module Database.TokyoDystopia.Class
    , module Database.TokyoDystopia.Types
    , module Database.TokyoDystopia.Utils
    ) where

import Database.TokyoDystopia.Class
import Database.TokyoDystopia.Types
import Database.TokyoDystopia.Utils

import qualified Database.TokyoDystopia.QDB as QDB
import qualified Database.TokyoDystopia.JDB as JDB
import qualified Database.TokyoDystopia.IDB as IDB
import qualified Database.TokyoDystopia.WDB as WDB
