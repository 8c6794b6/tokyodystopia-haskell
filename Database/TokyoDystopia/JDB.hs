------------------------------------------------------------------------------
-- |
-- Module      : Data.TokyoDystopia.JDB
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell binding for tokyodystopia TCJDB interface.
--

module Database.TokyoDystopia.JDB
    ( JDB()
    , close
    , copy
    , del
    , ecode
    , fsiz
    , get
    , get2
    , iterinit
    , iternext
    , new
    , open
    , optimize
    , path
    , put
    , put2
    , rnum
    , search
    , search2
    , setcache
    , setfwmmax
    , sync
    , tune
    , vanish
    , setdbgfd
    , getdbgfd
    , memsync
    , inode
    , mtime
    , opts
    , setsynccb
    , setexopts
    ) where

import Data.Int ( Int64 )
import Data.ByteString ( ByteString )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Foreign ( Ptr, maybePeek )
import Foreign.ForeignPtr ( withForeignPtr )

import Database.TokyoCabinet ( ECODE )
import Database.TokyoCabinet.List.C ( List(..) )
import Database.TokyoCabinet.Sequence ( peekList' )
import Database.TokyoCabinet.Storable ( Storable )

import Database.TokyoDystopia.Internal ( bitOr, toTuningOptions )
import Database.TokyoDystopia.Types
    ( OpenMode(..)
    , GetMode(..)
    , TuningOption(..) )

import qualified Foreign.C.String as CS
import qualified Data.ByteString.Char8 as C8

import qualified Database.TokyoCabinet.Error as TCE
import qualified Database.TokyoCabinet.Storable as TCS

import qualified Database.TokyoDystopia.FFI.JDB as FJ
import qualified Database.TokyoDystopia.Internal as I

-- | Wrapper for TCJDB
newtype JDB = JDB { unJDB :: Ptr FJ.TCJDB }

-- | OPen database from given path and open modes.
open :: JDB -> FilePath -> [OpenMode] -> IO Bool
open = I.mkOpen FJ.c_open unJDB (FJ.unOpenMode . f)
    where
      f OREADER = FJ.omReader
      f OWRITER = FJ.omWriter
      f OCREAT  = FJ.omCreat
      f OTRUNC  = FJ.omTrunc
      f ONOLCK  = FJ.omNolck
      f OLCKNB  = FJ.omLcknb

-- | Closes database
close :: JDB -> IO Bool
close = FJ.c_close . unJDB

copy :: JDB -> FilePath -> IO Bool
copy db file = CS.withCString file $ \file' -> FJ.c_copy (unJDB db) file'

del :: JDB -> IO ()
del = FJ.c_del . unJDB

ecode :: JDB -> IO ECODE
ecode = fmap TCE.cintToError . FJ.c_ecode . unJDB

fsiz :: JDB -> IO Int64
fsiz = FJ.c_fsiz . unJDB

get :: (Storable k) => JDB -> k -> IO (List ByteString)
get db key = do
  ptr' <- FJ.c_get (unJDB db) (TCS.toInt64 key)
  peekList' ptr'

-- | Get the value with given key.
-- Each word in returned String is separated by tab.
get2 :: (Storable k) => JDB -> k -> IO (Maybe ByteString)
get2 db key = do
  ptr <- FJ.c_get2 (unJDB db) (TCS.toInt64 key)
  maybePeek C8.packCString ptr

iterinit :: JDB -> IO Bool
iterinit = FJ.c_iterinit . unJDB

iternext :: JDB -> IO Int64
iternext = FJ.c_iternext . unJDB

-- | Creates new JDB
new :: IO JDB
new = JDB `fmap` FJ.c_new

optimize :: JDB -> IO Bool
optimize = FJ.c_optimize . unJDB

path :: JDB -> IO FilePath
path db = FJ.c_path (unJDB db) >>= CS.peekCString

-- | Put data with given key and values
put :: (Storable k) => JDB -> k -> List a -> IO Bool
put db k vs = do
  withForeignPtr (unTCList vs) (\v -> FJ.c_put (unJDB db) (TCS.toInt64 k) v)

-- | Put with specifying delimiter.
put2 :: (Storable k)
     => JDB        -- ^ JDB database
     -> k          -- ^ Key for the record
     -> ByteString -- ^ Value separated by delimiter
     -> ByteString -- ^ Delimiter
     -> IO Bool
put2 db key vs v = do
  C8.useAsCString vs $ \vs' ->
    C8.useAsCString v $ \v' ->
      FJ.c_put2 (unJDB db) (TCS.toInt64 key) vs' v'

rnum :: JDB -> IO Int64
rnum = FJ.c_rnum . unJDB

search :: JDB -> String -> [GetMode] -> IO [Int64]
search = I.mkSearch FJ.c_search unJDB g
  where
    g = bitOr . map (FJ.unGetMode . f)
    f GMSUBSTR = FJ.gmSubstr
    f GMPREFIX = FJ.gmPrefix
    f GMSUFFIX = FJ.gmSuffix
    f GMFULL   = FJ.gmFull
    f _        = FJ.GetMode 0

search2 :: JDB -> String -> IO [Int64]
search2 = I.mkSearch2 FJ.c_search2 unJDB

setcache :: JDB -> Int64 -> Int -> IO Bool
setcache db icsiz lcnum = FJ.c_setcache (unJDB db) icsiz (fromIntegral lcnum)

setfwmmax :: JDB -> Int -> IO Bool
setfwmmax db fwmmax = FJ.c_setfwmmax (unJDB db) (fromIntegral fwmmax)

sync :: JDB -> IO Bool
sync = FJ.c_sync . unJDB

tune :: JDB -> Int64 -> Int64 -> Int64 -> [TuningOption] -> IO Bool
tune db ernum etnum iusiz os =
  FJ.c_tune (unJDB db) ernum etnum iusiz os'
  where
    os' = fromIntegral . bitOr $ map (FJ.unTuningOption . f) os
    f TLARGE   = FJ.toLarge
    f TDEFLATE = FJ.toDeflate
    f TBZIP    = FJ.toBzip
    f TTCBS    = FJ.toTcbs
    f _        = FJ.TuningOption 0

vanish :: JDB -> IO Bool
vanish = FJ.c_vanish . unJDB


--
-- Advanced features
--

-- | Set file descriptor for debugging output.
setdbgfd :: JDB -> Int -> IO ()
setdbgfd db dbg = FJ.c_setdbgfd (unJDB db) (fromIntegral dbg)

-- | Get file descriptor for debugging output
getdbgfd :: JDB -> IO Int
getdbgfd = fmap fromIntegral . FJ.c_dbgfd . unJDB

-- | Synchronize updating contents on memory of an indexed database object
memsync :: JDB -> Int -> IO Bool
memsync db level = FJ.c_memsync (unJDB db) (fromIntegral level)

-- | Get the inode number of the database dictionary of an indexed database
-- object.
inode :: JDB -> IO Int64
inode = FJ.c_inode . unJDB

-- | Get the modificatoin time of the database directory of an indexed database
-- object.
mtime :: JDB -> IO UTCTime
mtime = fmap (posixSecondsToUTCTime . realToFrac) . FJ.c_mtime . unJDB

-- | Get the options of an indexed database object.
opts :: JDB -> IO [TuningOption]
opts = fmap toTuningOptions . FJ.c_opts . unJDB

-- | Set the callback function for sync progression.
setsynccb :: JDB -> (Int -> Int -> String -> IO Bool) -> IO Bool
setsynccb db cb = do
  cb' <- FJ.c_setsynccb_wrapper (\i1 i2 s -> do
                                    s' <- CS.peekCString s
                                    let i1' = fromIntegral i1
                                        i2' = fromIntegral i2
                                    cb i1' i2' s')
  FJ.c_setsynccb (unJDB db) cb'

-- | Set the expert options.
setexopts :: JDB -> [FJ.ExpertOption] -> IO ()
setexopts db os = FJ.c_setexopts (unJDB db) . fromIntegral . sum $
                  map FJ.unExpertOption os