------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.IDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCIDB interface.
--

module Database.TokyoDystopia.IDB
    ( -- * Type
      IDB()

      -- * Basic functions
    , close
    , copy
    , del
    , ecode
    , fsiz
    , get
    , iterinit
    , iternext
    , new
    , open
    , optimize
    , out
    , path
    , put
    , rnum
    , search
    , search2
    , setcache
    , setfwmmax
    , sync
    , tune
    , vanish

    -- * Advanced functions
    , setdbgfd
    , getdbgfd
    , memsync
    , inode
    , mtime
    , opts
    , setsynccb
    , setexopts
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign (Ptr, maybePeek)
import Database.TokyoCabinet.Storable (Storable)
import Database.TokyoCabinet (ECODE(..))
import Database.TokyoDystopia.Internal (bitOr, toTuningOptions)
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , GetMode(..)
    , TuningOption(..) )
import qualified Data.ByteString.Char8 as C8
import qualified Foreign.C.String as CS
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS
import qualified Database.TokyoDystopia.FFI.IDB as FI
import qualified Database.TokyoDystopia.Internal as I


-- | Wrapper for TCIDB.
newtype IDB = IDB { unIDB :: Ptr FI.TCIDB }

-- | Creates new IDB.
new :: IO IDB
new = IDB `fmap` FI.c_new

-- | Open database from given path and open modes.
open :: IDB -> FilePath -> [OpenMode] -> IO Bool
open = I.mkOpen FI.c_open unIDB (FI.unOpenMode . f)
  where
      f OREADER = FI.omReader
      f OWRITER = FI.omWriter
      f OCREAT  = FI.omCreat
      f OTRUNC  = FI.omTrunc
      f ONOLCK  = FI.omNolck
      f OLCKNB  = FI.omLcknb

-- | Closes database
close :: IDB -> IO Bool
close = FI.c_close . unIDB

-- | Put data with given key and value.
put :: (Storable k) => IDB -> k -> ByteString -> IO Bool
put db k v = C8.useAsCString v
             (\str -> FI.c_put (unIDB db) (TCS.toInt64 k) str)

-- | Get data with given key.
get :: (Storable k, Storable v) => IDB -> k -> IO (Maybe v)
get db i = do
  val <- FI.c_get (unIDB db) (TCS.toInt64 i)
  str <- maybePeek CS.peekCString val
  return $ fmap TCS.fromString str

-- | Search with GetMode options.
search :: IDB -> String -> [GetMode] -> IO [Int64]
search = I.mkSearch FI.c_search unIDB g
      where
        g = bitOr . map (FI.unGetMode . f)
        f GMSUBSTR = FI.gmSubstr
        f GMPREFIX = FI.gmPrefix
        f GMSUFFIX = FI.gmSuffix
        f GMFULL   = FI.gmFull
        f GMTOKEN  = FI.gmToken
        f GMTOKPRE = FI.gmTokPre
        f GMTOKSUF = FI.gmTokSuf

-- | Search with given query and returns list of id keys.
search2 :: IDB -> String -> IO [Int64]
search2 = I.mkSearch2 FI.c_search2 unIDB

-- | Delete database, from memory.
del :: IDB -> IO ()
del = FI.c_del . unIDB

-- | Get the last happened error code of an indexed database object.
ecode :: IDB -> IO ECODE
ecode db = fmap TCE.cintToError (FI.c_ecode $ unIDB db)

-- | Tune the database. Must be used before opening database.
tune :: IDB -> Int64 -> Int64 -> Int64 -> [TuningOption] -> IO Bool
tune db ernum etnum iusiz os =
    FI.c_tune (unIDB db) ernum etnum iusiz os'
    where
      os' = fromIntegral $ bitOr $ map (FI.unTuningOption . f) os
      f TLARGE   = FI.toLarge
      f TDEFLATE = FI.toDeflate
      f TBZIP    = FI.toBzip
      f TTCBS    = FI.toTcbs
      f _        = FI.TuningOption 0

-- | Set caching parameters. Must be used before opening database.
setcache :: IDB -> Int64 -> Int -> IO Bool
setcache db icsiz lcnum = FI.c_setcache (unIDB db) icsiz (fromIntegral lcnum)

-- | Set maximum number of forward matching expansion. Must be used before
-- opening database.
setfwmmax :: IDB -> Int -> IO Bool
setfwmmax db fwmmax = FI.c_setfwmmax (unIDB db) (fromIntegral fwmmax)

-- | Initialize the iterator.
iterinit :: IDB -> IO Bool
iterinit = FI.c_iterinit . unIDB

-- | Get next key for iterator
iternext :: IDB -> IO Int64
iternext =  FI.c_iternext . unIDB

-- | Sync database.
sync :: IDB -> IO Bool
sync = FI.c_sync . unIDB

-- | Optimize database.
optimize :: IDB -> IO Bool
optimize = FI.c_optimize . unIDB

-- | Removes record with given key
out :: (Storable k) => IDB -> k -> IO Bool
out db key = FI.c_out (unIDB db) (TCS.toInt64 key)

-- | Delete the database from disk.
vanish :: IDB -> IO Bool
vanish = FI.c_vanish . unIDB

-- | Copy the database to given filepath.
copy :: IDB -> FilePath -> IO Bool
copy db file = FI.c_copy (unIDB db) =<< CS.newCString file

-- | Get filepath of the database
path :: IDB -> IO FilePath
path db = FI.c_path (unIDB db) >>= CS.peekCString

-- | Get number of records in database.
rnum :: IDB -> IO Int64
rnum = FI.c_rnum . unIDB

-- | Get filesize of the database.
fsiz :: IDB -> IO Int64
fsiz = FI.c_fsiz . unIDB

--
-- Advanced features
--

-- | Set file descriptor for debugging output.
setdbgfd :: IDB -> Int -> IO ()
setdbgfd db dbg = FI.c_setdbgfd (unIDB db) (fromIntegral dbg)

-- | Get file descriptor for debugging output
getdbgfd :: IDB -> IO Int
getdbgfd = fmap fromIntegral . FI.c_dbgfd . unIDB

-- | Synchronize updating contents on memory of an indexed database object
memsync :: IDB -> Int -> IO Bool
memsync db level = FI.c_memsync (unIDB db) (fromIntegral level)

-- | Get the inode number of the database dictionary of an indexed database
-- object.
inode :: IDB -> IO Int64
inode = FI.c_inode . unIDB

-- | Get the modificatoin time of the database directory of an indexed database
-- object.
mtime :: IDB -> IO UTCTime
mtime = fmap (posixSecondsToUTCTime . realToFrac) . FI.c_mtime . unIDB

-- | Get the options of an indexed database object.
opts :: IDB -> IO [TuningOption]
opts = fmap toTuningOptions . FI.c_opts . unIDB

-- | Set the callback function for sync progression.
setsynccb :: IDB -> (Int -> Int -> String -> IO Bool) -> IO Bool
setsynccb db cb = do
  cb' <- FI.c_setsynccb_wrapper (\i1 i2 s -> do
                                    s' <- CS.peekCString s
                                    let i1' = fromIntegral i1
                                        i2' = fromIntegral i2
                                    cb i1' i2' s')
  FI.c_setsynccb (unIDB db) cb'

-- | Set the expert options.
setexopts :: IDB -> [FI.ExpertOption] -> IO ()
setexopts db os = FI.c_setexopts (unIDB db) . fromIntegral . sum $
                  map FI.unExpertOption os