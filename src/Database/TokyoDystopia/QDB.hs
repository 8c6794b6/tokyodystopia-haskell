------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.QDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCQDB interface.
--

module Database.TokyoDystopia.QDB
    (
    -- * Type
      QDB()
    , RSET()
    , IDSET()

    -- * Basic functions
    , close
    , copy
    , del
    , ecode
    , fsiz
    , new
    , open
    , optimize
    , out
    , path
    , put
    , search
    , setcache
    , setfwmmax
    , sync
    , tnum
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
    , getfwmmax
    , cnum
    , resunion
    , resisect
    , resdiff
    , textnormalize
    , idsetnew
    , idsetdel
    , idsetmark
    , idsetcheck
    , idsetclear
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Foreign (Ptr)
import Database.TokyoCabinet (ECODE(..))
import Database.TokyoCabinet.Storable (Storable)
import Database.TokyoDystopia.Internal (bitOr, toTuningOptions)
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , TuningOption(..)
    , GetMode(..) )
import qualified Data.ByteString.Char8 as C8
import qualified Foreign.C.String as CS
import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS
import qualified Database.TokyoDystopia.FFI.QDB as FQ
import qualified Database.TokyoDystopia.Internal as I

-- | Wrapper for TCQDB.
newtype QDB = QDB { unQDB :: Ptr FQ.TCQDB }

-- | Wrapper for QDBRSET.
newtype RSET = RSET { unRSET :: Ptr FQ.QDBRSET }

-- | Wrapper for TCIDSET.
newtype IDSET = IDSET { unIDSET :: Ptr FQ.TCIDSET }

-- | Close database.
close :: QDB -> IO Bool
close = FQ.c_close . unQDB

-- | Open database from given path and open modes.
open :: QDB -> FilePath -> [OpenMode] -> IO Bool
open = I.mkOpen FQ.c_open unQDB (FQ.unOpenMode . f)
  where
      f OREADER = FQ.omReader
      f OWRITER = FQ.omWriter
      f OCREAT  = FQ.omCreat
      f OTRUNC  = FQ.omTrunc
      f ONOLCK  = FQ.omNolck
      f OLCKNB  = FQ.omLcknb

-- | Delete database from memory.
del :: QDB -> IO ()
del = FQ.c_del . unQDB

-- | Get the last happened error code of database.
ecode :: QDB -> IO ECODE
ecode db = fmap TCE.cintToError (FQ.c_ecode $ unQDB db)

-- | Get file size.
fsiz :: QDB -> IO Int64
fsiz = FQ.c_fsiz . unQDB

-- | Creates new QDB.
new :: IO QDB
new = fmap QDB FQ.c_new

-- | Optimize database.
optimize :: QDB -> IO Bool
optimize = FQ.c_optimize . unQDB

-- | Removes record with given key.
out :: (Storable k) => QDB -> k -> String -> IO Bool
out db key val = do
  val' <- CS.newCString val
  FQ.c_out (unQDB db) (TCS.toInt64 key) val'

-- | Get filepath of the database
path :: QDB -> IO String
path db = FQ.c_path (unQDB db) >>= CS.peekCString

-- | Put data with given key and value.
put :: (Storable k) => QDB -> k -> ByteString -> IO Bool
put db k v = C8.useAsCString v
             (\str -> FQ.c_put (unQDB db) (TCS.toInt64 k) str)

-- | Get the number of token from database.
tnum :: QDB -> IO Int64
tnum = FQ.c_tnum . unQDB

-- | Search phrase with given GetMode.
search :: QDB -> String -> [GetMode] -> IO [Int64]
search = I.mkSearch FQ.c_search unQDB g
    where
      g = bitOr . map (FQ.unGetMode . f)
      f GMSUBSTR = FQ.gmSubstr
      f GMPREFIX = FQ.gmPrefix
      f GMSUFFIX = FQ.gmSuffix
      f GMFULL   = FQ.gmFull
      f _        = FQ.GetMode 0

-- | Set caching parameters. Must be used before opening database.
setcache :: QDB -> Int64 -> Int -> IO Bool
setcache db ic lc  = FQ.c_setcache (unQDB db) ic (fromIntegral lc)

-- | Set maximum number of forward matching expansion. Must be used before
-- opening database.
setfwmmax :: QDB -> Int -> IO Bool
setfwmmax db fwm = FQ.c_setfwmmax (unQDB db) (fromIntegral fwm)

-- | Sync database.
sync :: QDB -> IO Bool
sync = FQ.c_sync . unQDB

-- | Tune the database. Must be used before opening database.
tune :: QDB -> Int64 -> [TuningOption] -> IO Bool
tune db etnum os = FQ.c_tune (unQDB db) etnum os'
  where
    os' = fromIntegral . bitOr . map (FQ.unTuningOption . f) $ os
    f TLARGE   = FQ.toLarge
    f TDEFLATE = FQ.toDeflate
    f TBZIP    = FQ.toBzip
    f TTCBS    = FQ.toTcbs
    f _        = FQ.TuningOption 0

-- | Delete the database from disk.
vanish :: QDB -> IO Bool
vanish = FQ.c_vanish . unQDB

-- | Copy the database to given filepath.
copy :: QDB -> FilePath -> IO Bool
copy db file = do
  file' <- CS.newCString file
  FQ.c_copy (unQDB db) file'

--
-- Advanced features
--

-- | Set file descriptor for debugging output.
setdbgfd :: QDB -> Int -> IO ()
setdbgfd db dbg = FQ.c_setdbgfd (unQDB db) (fromIntegral dbg)

-- | Get file descriptor for debugging output
getdbgfd :: QDB -> IO Int
getdbgfd = fmap fromIntegral . FQ.c_dbgfd . unQDB

-- | Synchronize updating contents on memory of an indexed database object
memsync :: QDB -> Int -> IO Bool
memsync db level = FQ.c_memsync (unQDB db) (fromIntegral level)

-- | Get the inode number of the database dictionary of an indexed database
-- object.
inode :: QDB -> IO Int64
inode = FQ.c_inode . unQDB

-- | Get the modificatoin time of the database directory of an indexed database
-- object.
mtime :: QDB -> IO UTCTime
mtime = fmap (posixSecondsToUTCTime . realToFrac) . FQ.c_mtime . unQDB

-- | Get the options of an indexed database object.
opts :: QDB -> IO [TuningOption]
opts = fmap toTuningOptions . FQ.c_opts . unQDB

-- | Set the callback function for sync progression.
setsynccb :: QDB -> (Int -> Int -> String -> IO Bool) -> IO Bool
setsynccb db cb = do
  cb' <- FQ.c_setsynccb_wrapper (\i1 i2 s -> do
                                    s' <- CS.peekCString s
                                    let i1' = fromIntegral i1
                                        i2' = fromIntegral i2
                                    cb i1' i2' s')
  FQ.c_setsynccb (unQDB db) cb'

-- | Get maximum number of forward matching expansion.
getfwmmax :: QDB -> IO Int
getfwmmax = fmap fromIntegral . FQ.c_fwmmax . unQDB

-- | Get the number of records in the cache.
cnum :: QDB -> IO Int
cnum = fmap fromIntegral . FQ.c_cnum . unQDB

-- | Merge multiple result sets by union.
resunion :: RSET -> IO RSET
resunion _ = error "resunion not implemented yet"

-- | Merge multiple result sets by intersection.
resisect :: RSET -> IO RSET
resisect _ = error "resisect not implemented yet"

-- | Merge multiple result sets by difference.
resdiff :: RSET -> IO RSET
resdiff _ = error "resdiff not implemented yet"

textnormalize :: String -> [FQ.TNOption] -> IO String
textnormalize str os = do
  let os' = sum $ map FQ.unTNOption os
  str' <- CS.newCString str
  FQ.c_textnormalize str' (fromIntegral os')
  CS.peekCString str'

-- | Create an ID set object.
idsetnew :: Int -> IO IDSET
idsetnew = fmap IDSET . FQ.c_tcidsetnew . fromIntegral

-- | Delete an ID set object.
idsetdel :: IDSET -> IO ()
idsetdel = FQ.c_tcidsetdel . unIDSET

-- | Mark an ID number of an ID set object.
idsetmark :: IDSET -> Int -> IO ()
idsetmark set i = FQ.c_tcidsetmark (unIDSET set) (fromIntegral i)

-- | Check an ID of an ID set object.
idsetcheck :: IDSET -> Int -> IO Bool
idsetcheck set i = FQ.c_tcidsetcheck (unIDSET set) (fromIntegral i)

-- | Clear an ID set object.
idsetclear :: IDSET -> IO ()
idsetclear = FQ.c_tcidsetclear . unIDSET
