{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- |
-- Inner guts of haskell binding for tcwdb.h.
--
module Database.TokyoDystopia.FFI.WDB
    (
    -- * Types and constants

    -- ** Database
      TCWDB

    -- ** OpenMode
    , OpenMode(..)
    , omReader
    , omWriter
    , omCreat
    , omTrunc
    , omNolck
    , omLcknb

    -- ** GetMode
    , GetMode(..)
    , gmSubstr
    , gmPrefix
    , gmSuffix
    , gmFull

    -- ** Tuning options
    , TuningOption(..)
    , toLarge
    , toDeflate
    , toBzip
    , toTcbs

    -- * C Functions

    -- ** Basic functions
    , c_errmsg
    , c_new
    , c_del
    , c_ecode
    , c_tune
    , c_setcache
    , c_setfwmmax
    , c_cnum
    , c_open
    , c_close
    , c_put
    , c_put2
    , c_out
    , c_out2
    , c_search
    , c_optimize
    , c_vanish
    , c_copy
    , c_path
    , c_tnum
    , c_fsiz
    , c_sync

    -- ** Advanced functions
    , c_setdbgfd
    , c_dbgfd
    , c_memsync
    , c_cacheclear
    , c_inode
    , c_mtime
    , c_opts
    , c_fwmmax
    , c_setsynccb
    , c_setsynccb_wrapper
    , c_setaddcb
    , c_setaddcb_wrapper
    ) where

import Data.Int (Int32, Int64)
import Foreign (Ptr, FunPtr)
import Foreign.C.Types (CInt, CTime, CUInt)
import Foreign.C.String (CString)

import Database.TokyoCabinet.List.C (LIST)

#include <tcwdb.h>

------------------------------------------------------------------------------
--
-- Haskell Side Datatype definitions
--
------------------------------------------------------------------------------

data OpenMode = OpenMode { unOpenMode :: CInt }
              deriving (Eq, Show)

data GetMode = GetMode { unGetMode :: CInt }
             deriving (Eq, Show)

data TuningOption = TuningOption { unTuningOption :: CInt }
               deriving (Eq, Show)

data TCWDB

------------------------------------------------------------------------------
--
-- Enums
--
------------------------------------------------------------------------------

#{enum OpenMode, OpenMode
 , omReader = QDBOREADER
 , omWriter = QDBOWRITER
 , omCreat = QDBOCREAT
 , omTrunc  = QDBOTRUNC
 , omNolck  = QDBONOLCK
 , omLcknb  = QDBOLCKNB }

#{enum GetMode, GetMode
 , gmSubstr = QDBSSUBSTR
 , gmPrefix = QDBSPREFIX
 , gmSuffix = QDBSSUFFIX
 , gmFull = QDBSFULL }

#{enum TuningOption, TuningOption
 , toLarge = QDBTLARGE
 , toDeflate = QDBTDEFLATE
 , toBzip = QDBTBZIP
 , toTcbs = QDBTTCBS }

------------------------------------------------------------------------------
--
-- Function calls
--
------------------------------------------------------------------------------

foreign import ccall "tcwdb.h tcwdberrmsg"
  c_errmsg :: CInt -> CString

foreign import ccall "tcwdb.h tcwdbnew"
  c_new :: IO (Ptr TCWDB)

foreign import ccall "tcwdb.h tcwdbdel"
  c_del :: Ptr TCWDB -> IO ()

foreign import ccall "tcwdb.h tcwdbecode"
  c_ecode :: Ptr TCWDB -> IO CInt

foreign import ccall "tcwdb.h tcwdbtune"
  c_tune :: Ptr TCWDB -> Int64 -> CUInt -> IO Bool

foreign import ccall "tcwdb.h tcwdbsetcache"
  c_setcache :: Ptr TCWDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "tcwdb.h tcwdbsetfwmmax"
  c_setfwmmax :: Ptr TCWDB -> Int32 -> IO Bool

foreign import ccall "tcwdb.h tcwdbcnum"
  c_cnum :: Ptr TCWDB -> IO CUInt

foreign import ccall "tcwdb.h tcwdbopen"
  c_open :: Ptr TCWDB -> CString -> CInt -> IO Bool

foreign import ccall "tcwdb.h tcwdbclose"
  c_close :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbput"
  c_put :: Ptr TCWDB -> Int64 -> Ptr LIST -> IO Bool

foreign import ccall "tcwdb.h tcwdbput2"
  c_put2 :: Ptr TCWDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbout"
  c_out :: Ptr TCWDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbout2"
  c_out2 :: Ptr TCWDB -> Int64 -> CString -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbsearch"
  c_search :: Ptr TCWDB -> CString -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcwdb.h tcwdbsync"
  c_sync :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdboptimize"
  c_optimize :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbvanish"
  c_vanish :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbcopy"
  c_copy :: Ptr TCWDB -> CString -> IO Bool

foreign import ccall "tcwdb.h tcwdbpath"
  c_path :: Ptr TCWDB -> IO CString

foreign import ccall "tcwdb.h tcwdbtnum"
  c_tnum :: Ptr TCWDB -> IO Int64

foreign import ccall "tcwdb.h tcwdbfsiz"
  c_fsiz :: Ptr TCWDB -> IO Int64

--
-- Advanced functions
--

foreign import ccall "tcwdb.h tcwdbsetdbgfd"
  c_setdbgfd :: Ptr TCWDB -> CInt -> IO ()

foreign import ccall "tcwdb.h tcwdbdbgfd"
  c_dbgfd :: Ptr TCWDB -> IO CInt

foreign import ccall "tcwdb.h tcwdbmemsync"
  c_memsync :: Ptr TCWDB -> CInt -> IO Bool

foreign import ccall "tcwdb.h tcwdbcacheclear"
  c_cacheclear :: Ptr TCWDB -> IO Bool

foreign import ccall "tcwdb.h tcwdbinode"
  c_inode :: Ptr TCWDB -> IO Int64

foreign import ccall "tcwdb.h tcwdbmtime"
  c_mtime :: Ptr TCWDB -> IO CTime

foreign import ccall "tcwdb.h tcwdbopts"
  c_opts :: Ptr TCWDB -> IO Int

foreign import ccall "tcwdb.h tcwdbfwmmax"
  c_fwmmax :: Ptr TCWDB -> IO Int32

foreign import ccall "wrapper"
  c_setsynccb_wrapper :: (CInt -> CInt -> CString -> IO Bool)
                      -> IO (FunPtr (CInt -> CInt -> CString -> IO Bool))

foreign import ccall "tcwdb.h tcwdbsetsynccb"
  c_setsynccb :: Ptr TCWDB -> FunPtr (CInt -> CInt -> CString -> IO Bool) -> IO Bool

foreign import ccall "wrapper"
  c_setaddcb_wrapper :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "tcwdb.h tcwdbsetaddcb"
  c_setaddcb :: Ptr TCWDB -> FunPtr (CString -> IO ()) -> IO ()
